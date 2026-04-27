#!/usr/bin/env python3
"""
Interactive and batch pullout simulation for the squareAndPole STEP assembly.

Interactive mode provides:
- Run / pause solve
- Playback after the solve finishes
- Reset simulation
- Exact-value numeric entry for controls
- Grain-size control with rebuild-on-run behavior
- Adaptive fine/coarse DEM soil generation for small-particle studies
- Solve-speed control
- Pull-rate control
- Force readout with a stop threshold
- Pull-height and pull-angle control
- Scrollable controls, a viewport, and a force-displacement plot

Batch mode still exports CSV/PNG/MP4 results for scripted runs.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import multiprocessing as mp
import pickle
import re
import tempfile
import time
import traceback
from dataclasses import dataclass, replace
from datetime import datetime
from pathlib import Path
from queue import Empty
from typing import Any, Dict, Iterable, List, Optional, Tuple

import gmsh
import matplotlib.pyplot as plt
import numpy as np
import pybullet as p
from matplotlib.figure import Figure
from scipy.spatial.transform import Rotation

try:
    import tkinter as tk
    from tkinter import ttk
except ImportError:  # pragma: no cover - platform dependent
    tk = None
    ttk = None

try:
    from PIL import Image, ImageTk
except ImportError:  # pragma: no cover - optional for UI
    Image = None
    ImageTk = None

try:
    from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
except ImportError:  # pragma: no cover - optional for UI
    FigureCanvasTkAgg = None

try:
    import cv2
except ImportError:  # pragma: no cover - optional runtime dependency
    cv2 = None


INCH_TO_M = 0.0254
LBF_PER_N = 0.2248089431
N_PER_LBF = 1.0 / LBF_PER_N
FLOAT_RE = re.compile(r"[-+]?(?:\d+\.\d*|\d+|\.\d+)(?:[Ee][-+]?\d+)?")
MIN_GRAIN_RADIUS_IN = 0.01
DEFAULT_MAX_SOIL_PARTICLES = 0
LARGE_SOIL_WARNING_PARTICLES = 80000
AUTO_PREVIEW_SOIL_PARTICLES = 4000
DEFAULT_COARSE_PARTICLE_RADIUS_IN = 0.35
DEFAULT_FINE_ZONE_MARGIN_IN = 1.5
DEFAULT_MAX_PLAYBACK_MEMORY_MB = 768.0


@dataclass
class BoundingBox:
    mins: np.ndarray
    maxs: np.ndarray

    @property
    def dims(self) -> np.ndarray:
        return self.maxs - self.mins

    @property
    def center(self) -> np.ndarray:
        return 0.5 * (self.mins + self.maxs)

    def scaled(self, factor: float) -> "BoundingBox":
        return BoundingBox(mins=self.mins * factor, maxs=self.maxs * factor)


@dataclass
class Placement:
    origin: np.ndarray
    rotation: np.ndarray

    @property
    def matrix(self) -> np.ndarray:
        transform = np.eye(4)
        transform[:3, :3] = self.rotation
        transform[:3, 3] = self.origin
        return transform


@dataclass
class ComponentGeometry:
    filename: str
    role: str
    bbox: BoundingBox
    transform_world: np.ndarray
    rel_position: np.ndarray
    rel_rotation: np.ndarray

    @property
    def dims(self) -> np.ndarray:
        return self.bbox.dims

    @property
    def half_extents(self) -> np.ndarray:
        return 0.5 * self.dims


@dataclass
class AnchorAssembly:
    base: ComponentGeometry
    links: List[ComponentGeometry]
    surface_y: float
    base_bottom_y: float
    rod_tip_y: float


@dataclass
class SimulationParameters:
    pull_distance_in: float = 6.5
    pull_speed_in_per_s: float = 1.0
    object_mass_lb: float = 10.2
    particle_radius_in: float = 0.8
    particle_density: float = 1700.0
    soil_lateral_margin_in: float = 5.0
    soil_bottom_margin_in: float = 2.5
    settle_time_s: float = 0.5
    time_step: float = 1.0 / 240.0
    random_seed: int = 7
    capture_every: int = 8
    pull_force_lbf: float = 1200.0
    pull_height_in: float = 17.5
    pull_angle_deg: float = 0.0
    simulation_speed: float = 1.0
    soil_mode: str = "adaptive"
    coarse_particle_radius_in: float = DEFAULT_COARSE_PARTICLE_RADIUS_IN
    fine_zone_margin_in: float = DEFAULT_FINE_ZONE_MARGIN_IN
    max_soil_particles: int = DEFAULT_MAX_SOIL_PARTICLES
    max_playback_memory_mb: float = DEFAULT_MAX_PLAYBACK_MEMORY_MB


@dataclass
class PlaybackSnapshot:
    time_s: float
    commanded_displacement_m: float
    anchor_position: np.ndarray
    anchor_orientation: np.ndarray
    particle_positions: np.ndarray
    force_world: np.ndarray


def parse_args() -> argparse.Namespace:
    default_step = Path("poleGeometryStepFiles") / "squareAndPole.STEP"
    default_output = Path("outputs") / "pole_pullout_sim"

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--step-file", type=Path, default=default_step)
    parser.add_argument("--output-dir", type=Path, default=default_output)
    parser.add_argument(
        "--gui",
        "--interactive",
        dest="interactive",
        action="store_true",
        help="Launch the interactive desktop UI.",
    )
    parser.add_argument("--no-video", action="store_true", help="Skip MP4 export in batch mode.")
    parser.add_argument("--no-plot", action="store_true", help="Skip PNG plot export in batch mode.")
    parser.add_argument("--pull-distance-in", type=float, default=6.5)
    parser.add_argument("--pull-speed-in-per-s", type=float, default=1.0)
    parser.add_argument("--object-mass-lb", type=float, default=10.2)
    parser.add_argument("--particle-radius-in", type=float, default=0.80)
    parser.add_argument("--particle-density", type=float, default=1700.0)
    parser.add_argument("--soil-lateral-margin-in", type=float, default=5.0)
    parser.add_argument("--soil-bottom-margin-in", type=float, default=2.5)
    parser.add_argument("--settle-time-s", type=float, default=0.5)
    parser.add_argument("--time-step", type=float, default=1.0 / 240.0)
    parser.add_argument("--random-seed", type=int, default=7)
    parser.add_argument("--capture-every", type=int, default=8)
    parser.add_argument("--pull-force-lbf", type=float, default=1200.0)
    parser.add_argument("--pull-height-in", type=float, default=17.5)
    parser.add_argument("--pull-angle-deg", type=float, default=0.0)
    parser.add_argument("--simulation-speed", type=float, default=1.0)
    parser.add_argument(
        "--soil-mode",
        choices=("adaptive", "full"),
        default="adaptive",
        help=(
            "Soil particle layout. 'adaptive' keeps requested fine grains around the pull path "
            "and uses coarser far-field grains; 'full' uses the requested grain size everywhere."
        ),
    )
    parser.add_argument(
        "--coarse-particle-radius-in",
        type=float,
        default=DEFAULT_COARSE_PARTICLE_RADIUS_IN,
        help="Far-field particle radius for adaptive soil mode.",
    )
    parser.add_argument(
        "--fine-zone-margin-in",
        type=float,
        default=DEFAULT_FINE_ZONE_MARGIN_IN,
        help="Extra fine-grain margin around the anchor pull path in adaptive soil mode.",
    )
    parser.add_argument(
        "--max-soil-particles",
        type=int,
        default=DEFAULT_MAX_SOIL_PARTICLES,
        help="Optional hard particle cap. Use 0 to allow large runs.",
    )
    parser.add_argument(
        "--max-playback-memory-mb",
        type=float,
        default=DEFAULT_MAX_PLAYBACK_MEMORY_MB,
        help="Approximate memory budget for recorded GUI playback snapshots.",
    )
    return parser.parse_args()


def read_step_entities(step_path: Path) -> Dict[int, str]:
    text = step_path.read_text(encoding="utf-8", errors="ignore")
    data_match = re.search(r"DATA;(.*)ENDSEC;", text, flags=re.S)
    if not data_match:
        raise ValueError(f"Could not find DATA section in {step_path}")

    entries = re.findall(r"#(\d+)\s*=\s*(.*?);", data_match.group(1), flags=re.S)
    return {int(entity_id): content.strip() for entity_id, content in entries}


def infer_step_length_scale(step_path: Path) -> float:
    text = step_path.read_text(encoding="utf-8", errors="ignore")
    if "CONVERSION_BASED_UNIT ( 'INCH'" in text or "CONVERSION_BASED_UNIT ('INCH'" in text:
        return INCH_TO_M
    return 1.0


def quoted_strings(content: str) -> List[str]:
    return re.findall(r"'([^']*)'", content)


def entity_refs(content: str) -> List[int]:
    return [int(match) for match in re.findall(r"#(\d+)", content)]


def entity_floats(content: str) -> List[float]:
    return [float(match) for match in FLOAT_RE.findall(content)]


def normalize(vector: np.ndarray) -> np.ndarray:
    norm = np.linalg.norm(vector)
    if norm == 0.0:
        raise ValueError("Encountered a zero-length vector while building a placement.")
    return vector / norm


def placement_from_step_entity(
    entity_id: int,
    entities: Dict[int, str],
    points: Dict[int, np.ndarray],
    directions: Dict[int, np.ndarray],
) -> Placement:
    refs = entity_refs(entities[entity_id])
    if len(refs) < 3:
        raise ValueError(f"AXIS2_PLACEMENT_3D #{entity_id} is missing references.")

    point = points[refs[0]]
    axis_z = normalize(directions[refs[1]])
    axis_x_hint = normalize(directions[refs[2]])
    axis_y = normalize(np.cross(axis_z, axis_x_hint))
    axis_x = normalize(np.cross(axis_y, axis_z))
    rotation = np.column_stack([axis_x, axis_y, axis_z])
    return Placement(origin=point, rotation=rotation)


def load_assembly_transforms(step_path: Path) -> Dict[str, np.ndarray]:
    entities = read_step_entities(step_path)
    point_scale = infer_step_length_scale(step_path)

    document_files: Dict[int, str] = {}
    document_for_target: Dict[int, int] = {}
    next_usage_children: Dict[int, int] = {}
    pds_targets: Dict[int, int] = {}
    rep_relationship_transform: Dict[int, int] = {}
    transforms: Dict[int, Tuple[int, int]] = {}
    points: Dict[int, np.ndarray] = {}
    directions: Dict[int, np.ndarray] = {}

    for entity_id, content in entities.items():
        if content.startswith("DOCUMENT_FILE"):
            strings = quoted_strings(content)
            if strings:
                document_files[entity_id] = strings[0]
        elif content.startswith("APPLIED_DOCUMENT_REFERENCE"):
            refs = entity_refs(content)
            if len(refs) >= 2:
                document_id = refs[0]
                for target in refs[1:]:
                    document_for_target[target] = document_id
        elif content.startswith("NEXT_ASSEMBLY_USAGE_OCCURRENCE"):
            refs = entity_refs(content)
            if len(refs) >= 2:
                next_usage_children[entity_id] = refs[1]
        elif content.startswith("PRODUCT_DEFINITION_SHAPE"):
            refs = entity_refs(content)
            if refs:
                pds_targets[entity_id] = refs[-1]
        elif "REPRESENTATION_RELATIONSHIP_WITH_TRANSFORMATION" in content:
            transform_match = re.search(
                r"REPRESENTATION_RELATIONSHIP_WITH_TRANSFORMATION\s*\(\s*#(\d+)\s*\)",
                content,
            )
            if transform_match:
                rep_relationship_transform[entity_id] = int(transform_match.group(1))
        elif content.startswith("ITEM_DEFINED_TRANSFORMATION"):
            refs = entity_refs(content)
            if len(refs) >= 2:
                transforms[entity_id] = (refs[0], refs[1])
        elif content.startswith("CARTESIAN_POINT"):
            floats = entity_floats(content)
            points[entity_id] = np.array(floats[-3:], dtype=float) * point_scale
        elif content.startswith("DIRECTION"):
            floats = entity_floats(content)
            directions[entity_id] = np.array(floats[-3:], dtype=float)

    placements: Dict[int, Placement] = {}
    for entity_id, content in entities.items():
        if content.startswith("AXIS2_PLACEMENT_3D"):
            placements[entity_id] = placement_from_step_entity(entity_id, entities, points, directions)

    component_transforms: Dict[str, np.ndarray] = {}
    for entity_id, content in entities.items():
        if not content.startswith("CONTEXT_DEPENDENT_SHAPE_REPRESENTATION"):
            continue

        refs = entity_refs(content)
        if len(refs) < 2:
            continue

        rep_rel_id, pds_id = refs[0], refs[1]
        transform_id = rep_relationship_transform[rep_rel_id]
        target = pds_targets[pds_id]
        product_definition = next_usage_children.get(target, target)
        document_id = document_for_target[product_definition]
        filename = document_files[document_id]

        placement_global_id, placement_local_id = transforms[transform_id]
        transform_matrix = (
            placements[placement_global_id].matrix
            @ np.linalg.inv(placements[placement_local_id].matrix)
        )
        component_transforms.setdefault(filename, [])
        component_transforms[filename].append(transform_matrix)

    flat_transforms: Dict[str, np.ndarray] = {}
    for filename, matrices in component_transforms.items():
        if len(matrices) == 1:
            flat_transforms[filename] = matrices[0]
        else:
            for index, matrix in enumerate(matrices, start=1):
                flat_transforms[f"{Path(filename).stem}__instance_{index}{Path(filename).suffix}"] = matrix

    return flat_transforms


def component_source_for_instance(assembly_dir: Path, instance_name: str) -> Path:
    if "__instance_" in instance_name:
        stem, _ = instance_name.split("__instance_", maxsplit=1)
        return assembly_dir / f"{stem}{Path(instance_name).suffix}"
    return assembly_dir / instance_name


def gmsh_bbox(step_path: Path) -> BoundingBox:
    gmsh.clear()
    gmsh.model.add(step_path.stem)
    gmsh.model.occ.importShapes(str(step_path))
    gmsh.model.occ.synchronize()
    bbox = gmsh.model.getBoundingBox(-1, -1)
    mins = np.array(bbox[:3], dtype=float)
    maxs = np.array(bbox[3:], dtype=float)
    return BoundingBox(mins=mins, maxs=maxs).scaled(0.001)


def detect_role(name: str) -> str:
    lower = name.lower()
    if "square base" in lower:
        return "base"
    if "threaded rod" in lower or "rod" in lower:
        return "rod"
    if "nut" in lower:
        return "nut"
    return "other"


def rotation_quat_xyzw(matrix: np.ndarray) -> Tuple[float, float, float, float]:
    return tuple(Rotation.from_matrix(matrix).as_quat().tolist())


def load_anchor_assembly(step_path: Path) -> AnchorAssembly:
    assembly_dir = step_path.parent
    transforms = load_assembly_transforms(step_path)
    bbox_cache: Dict[Path, BoundingBox] = {}

    gmsh.initialize()
    gmsh.option.setNumber("General.Terminal", 0)
    try:
        components_world: List[Tuple[str, str, BoundingBox, np.ndarray, np.ndarray, np.ndarray]] = []
        for instance_name, transform_world in transforms.items():
            source_path = component_source_for_instance(assembly_dir, instance_name)
            bbox = bbox_cache.get(source_path)
            if bbox is None:
                bbox = gmsh_bbox(source_path)
                bbox_cache[source_path] = bbox
            role = detect_role(source_path.name)
            center_world = transform_world[:3, :3] @ bbox.center + transform_world[:3, 3]
            rotation_world = transform_world[:3, :3]
            components_world.append(
                (instance_name, role, bbox, transform_world, center_world, rotation_world)
            )
    finally:
        gmsh.finalize()

    base_candidates = [item for item in components_world if item[1] == "base"]
    if not base_candidates:
        raise RuntimeError("Could not find the square base in the assembly STEP.")

    _, _, _, _, base_center_world, base_rotation_world = base_candidates[0]
    base_to_world = np.eye(4)
    base_to_world[:3, :3] = base_rotation_world
    base_to_world[:3, 3] = base_center_world
    world_to_base = np.linalg.inv(base_to_world)

    components: List[ComponentGeometry] = []
    for instance_name, role, bbox, transform_world, center_world, rotation_world in components_world:
        center_base = (world_to_base @ np.append(center_world, 1.0))[:3]
        rotation_base = world_to_base[:3, :3] @ rotation_world
        components.append(
            ComponentGeometry(
                filename=instance_name,
                role=role,
                bbox=bbox,
                transform_world=transform_world,
                rel_position=center_base,
                rel_rotation=rotation_base,
            )
        )

    base_component = next(component for component in components if component.role == "base")
    link_components = [component for component in components if component is not base_component]
    base_bottom_y = -base_component.half_extents[1]
    surface_y = base_bottom_y + 6.0 * INCH_TO_M

    rod_candidates = [component for component in components if component.role == "rod"]
    rod_tip_y = max(
        (
            component.rel_position[1] + component.half_extents[np.argmax(component.dims)]
            for component in rod_candidates
        ),
        default=surface_y,
    )

    return AnchorAssembly(
        base=base_component,
        links=link_components,
        surface_y=surface_y,
        base_bottom_y=base_bottom_y,
        rod_tip_y=rod_tip_y,
    )


def volume_from_bbox(dims: np.ndarray) -> float:
    return float(np.prod(dims))


def pounds_to_kg(value_lb: float) -> float:
    return value_lb * 0.45359237


def format_numeric_value(value: float, decimals: int = 3) -> str:
    formatted = f"{value:.{decimals}f}"
    if "." not in formatted:
        return formatted
    return formatted.rstrip("0").rstrip(".")


def point_inside_component(local_point: np.ndarray, component: ComponentGeometry, padding: float) -> bool:
    rel = local_point - component.rel_position
    local_component = component.rel_rotation.T @ rel
    return np.all(np.abs(local_component) <= component.half_extents + padding)


def create_static_box(
    half_extents: Iterable[float],
    position: Iterable[float],
    rgba: Iterable[float],
) -> int:
    collision = p.createCollisionShape(p.GEOM_BOX, halfExtents=list(half_extents))
    visual = p.createVisualShape(p.GEOM_BOX, halfExtents=list(half_extents), rgbaColor=list(rgba))
    body = p.createMultiBody(
        baseMass=0.0,
        baseCollisionShapeIndex=collision,
        baseVisualShapeIndex=visual,
        basePosition=list(position),
    )
    p.changeDynamics(body, -1, lateralFriction=0.95, restitution=0.0)
    return body


def build_anchor(anchor: AnchorAssembly, total_mass_kg: float) -> Tuple[int, float]:
    steel_density = 7850.0
    base = anchor.base
    base_reference_mass = steel_density * volume_from_bbox(base.dims)
    reference_total_mass = base_reference_mass

    base_collision = p.createCollisionShape(p.GEOM_BOX, halfExtents=base.half_extents.tolist())
    base_visual = p.createVisualShape(
        p.GEOM_BOX,
        halfExtents=base.half_extents.tolist(),
        rgbaColor=[0.35, 0.40, 0.45, 1.0],
    )

    link_masses = []
    link_collision = []
    link_visual = []
    link_positions = []
    link_orientations = []
    link_inertial_positions = []
    link_inertial_orientations = []
    link_parent_indices = []
    link_joint_types = []
    link_joint_axes = []
    reference_link_masses = []

    for component in anchor.links:
        mass = steel_density * volume_from_bbox(component.dims)
        reference_total_mass += mass
        reference_link_masses.append(mass)

        color = [0.72, 0.72, 0.76, 1.0]
        if component.role == "rod":
            color = [0.55, 0.55, 0.58, 1.0]
        if component.role == "nut":
            color = [0.80, 0.74, 0.20, 1.0]

        link_masses.append(mass)
        link_collision.append(
            p.createCollisionShape(p.GEOM_BOX, halfExtents=component.half_extents.tolist())
        )
        link_visual.append(
            p.createVisualShape(
                p.GEOM_BOX,
                halfExtents=component.half_extents.tolist(),
                rgbaColor=color,
            )
        )
        link_positions.append(component.rel_position.tolist())
        link_orientations.append(rotation_quat_xyzw(component.rel_rotation))
        link_inertial_positions.append([0.0, 0.0, 0.0])
        link_inertial_orientations.append([0.0, 0.0, 0.0, 1.0])
        link_parent_indices.append(0)
        link_joint_types.append(p.JOINT_FIXED)
        link_joint_axes.append([0.0, 0.0, 0.0])

    mass_scale = 1.0
    if reference_total_mass > 1e-9:
        mass_scale = total_mass_kg / reference_total_mass
    base_mass = max(1e-6, base_reference_mass * mass_scale)
    link_masses = [max(1e-6, mass * mass_scale) for mass in reference_link_masses]

    anchor_body = p.createMultiBody(
        baseMass=base_mass,
        baseCollisionShapeIndex=base_collision,
        baseVisualShapeIndex=base_visual,
        basePosition=[0.0, 0.0, 0.0],
        baseOrientation=[0.0, 0.0, 0.0, 1.0],
        linkMasses=link_masses,
        linkCollisionShapeIndices=link_collision,
        linkVisualShapeIndices=link_visual,
        linkPositions=link_positions,
        linkOrientations=link_orientations,
        linkInertialFramePositions=link_inertial_positions,
        linkInertialFrameOrientations=link_inertial_orientations,
        linkParentIndices=link_parent_indices,
        linkJointTypes=link_joint_types,
        linkJointAxis=link_joint_axes,
    )

    for link_index in range(-1, p.getNumJoints(anchor_body)):
        p.changeDynamics(
            anchor_body,
            link_index,
            lateralFriction=1.1,
            rollingFriction=0.02,
            spinningFriction=0.02,
            linearDamping=0.04,
            angularDamping=0.08,
            restitution=0.0,
        )

    return anchor_body, total_mass_kg


def create_soil_and_container(
    anchor: AnchorAssembly,
    particle_radius: float,
    particle_density: float,
    lateral_margin: float,
    bottom_margin: float,
    rng: np.random.Generator,
    *,
    soil_mode: str = "adaptive",
    coarse_particle_radius: Optional[float] = None,
    fine_zone_margin: float = DEFAULT_FINE_ZONE_MARGIN_IN * INCH_TO_M,
    pull_distance: float = 0.0,
    pull_angle_deg: float = 0.0,
) -> Tuple[List[int], Dict[str, float]]:
    base = anchor.base
    soil_bottom = anchor.base_bottom_y - bottom_margin
    soil_half_x = max(base.half_extents[0] + lateral_margin, 0.14)
    soil_half_z = max(base.half_extents[2] + lateral_margin, 0.14)
    soil_height = anchor.surface_y - soil_bottom

    wall_thickness = 0.015
    wall_half_y = 0.5 * soil_height + 0.03
    wall_center_y = soil_bottom + 0.5 * soil_height - 0.015

    create_static_box(
        [soil_half_x + wall_thickness, wall_thickness, soil_half_z + wall_thickness],
        [0.0, soil_bottom - wall_thickness, 0.0],
        [0.45, 0.32, 0.20, 1.0],
    )
    create_static_box(
        [wall_thickness, wall_half_y, soil_half_z + wall_thickness],
        [soil_half_x + wall_thickness, wall_center_y, 0.0],
        [0.75, 0.75, 0.78, 0.22],
    )
    create_static_box(
        [wall_thickness, wall_half_y, soil_half_z + wall_thickness],
        [-soil_half_x - wall_thickness, wall_center_y, 0.0],
        [0.75, 0.75, 0.78, 0.22],
    )
    create_static_box(
        [soil_half_x + wall_thickness, wall_half_y, wall_thickness],
        [0.0, wall_center_y, soil_half_z + wall_thickness],
        [0.75, 0.75, 0.78, 0.22],
    )
    create_static_box(
        [soil_half_x + wall_thickness, wall_half_y, wall_thickness],
        [0.0, wall_center_y, -soil_half_z - wall_thickness],
        [0.75, 0.75, 0.78, 0.22],
    )

    fine_radius = max(1e-9, particle_radius)
    coarse_radius = max(fine_radius, coarse_particle_radius or fine_radius)
    use_adaptive = soil_mode == "adaptive" and coarse_radius > fine_radius * 1.01
    fine_zone = fine_zone_bounds(
        anchor=anchor,
        margin=max(0.0, fine_zone_margin),
        pull_distance=max(0.0, pull_distance),
        pull_angle_deg=pull_angle_deg,
    )
    shape_cache: Dict[Tuple[float, str], Tuple[int, int, float]] = {}
    particles: List[int] = []
    particle_counts = {"fine": 0, "coarse": 0}

    def particle_shape(radius: float, region: str) -> Tuple[int, int, float]:
        key = (round(radius, 9), region)
        if key not in shape_cache:
            color = [0.54, 0.39, 0.24, 1.0] if region == "fine" else [0.64, 0.51, 0.34, 1.0]
            collision = p.createCollisionShape(p.GEOM_SPHERE, radius=radius)
            visual = p.createVisualShape(p.GEOM_SPHERE, radius=radius, rgbaColor=color)
            volume = 4.0 / 3.0 * math.pi * radius**3
            shape_cache[key] = (collision, visual, particle_density * volume)
        return shape_cache[key]

    def in_soil_bounds(position: np.ndarray, radius: float) -> bool:
        return (
            -soil_half_x + radius <= position[0] <= soil_half_x - radius
            and soil_bottom + radius <= position[1] <= anchor.surface_y - radius
            and -soil_half_z + radius <= position[2] <= soil_half_z - radius
        )

    def in_fine_zone(position: np.ndarray, padding: float = 0.0) -> bool:
        min_x, max_x, min_z, max_z = fine_zone
        return (
            min_x - padding <= position[0] <= max_x + padding
            and min_z - padding <= position[2] <= max_z + padding
        )

    def blocked_by_anchor(position: np.ndarray, radius: float) -> bool:
        occupied = point_inside_component(position, anchor.base, padding=1.15 * radius)
        if occupied:
            return True
        return any(
            point_inside_component(position, component, padding=0.9 * radius)
            for component in anchor.links
        )

    def create_particle(position: np.ndarray, radius: float, region: str) -> None:
        collision, visual, mass = particle_shape(radius, region)
        body = p.createMultiBody(
            baseMass=mass,
            baseCollisionShapeIndex=collision,
            baseVisualShapeIndex=visual,
            basePosition=position.tolist(),
        )
        p.changeDynamics(
            body,
            -1,
            lateralFriction=0.95,
            rollingFriction=0.01,
            spinningFriction=0.02,
            restitution=0.0,
            linearDamping=0.01,
            angularDamping=0.01,
        )
        particles.append(body)
        particle_counts[region] += 1

    def generate_particles(radius: float, region: str) -> None:
        spacing = 2.08 * radius
        xs = np.arange(-soil_half_x + radius, soil_half_x - radius + 1e-9, spacing)
        ys = np.arange(soil_bottom + radius, anchor.surface_y - radius + 1e-9, spacing)
        zs = np.arange(-soil_half_z + radius, soil_half_z - radius + 1e-9, spacing)

        for iy, y in enumerate(ys):
            x_offset = (iy % 2) * radius
            for iz, z in enumerate(zs):
                z_offset = ((iy + iz) % 2) * radius * 0.5
                for x in xs:
                    position = np.array(
                        [
                            x + x_offset - 0.5 * radius,
                            y,
                            z + z_offset,
                        ],
                        dtype=float,
                    )
                    position += rng.uniform(-0.03 * radius, 0.03 * radius, size=3)
                    if not in_soil_bounds(position, radius):
                        continue
                    if region == "fine" and use_adaptive and not in_fine_zone(position):
                        continue
                    if region == "coarse" and in_fine_zone(position, padding=coarse_radius + fine_radius):
                        continue
                    if blocked_by_anchor(position, radius):
                        continue
                    create_particle(position, radius, region)

    if use_adaptive:
        generate_particles(fine_radius, "fine")
        generate_particles(coarse_radius, "coarse")
    else:
        generate_particles(fine_radius, "fine")

    return particles, {
        "soil_bottom": soil_bottom,
        "soil_half_x": soil_half_x,
        "soil_half_z": soil_half_z,
        "soil_height": soil_height,
        "soil_mode": "adaptive" if use_adaptive else "full",
        "fine_particle_radius_in": fine_radius / INCH_TO_M,
        "coarse_particle_radius_in": coarse_radius / INCH_TO_M if use_adaptive else fine_radius / INCH_TO_M,
        "fine_zone_margin_in": fine_zone_margin / INCH_TO_M,
        "fine_zone_min_x_m": fine_zone[0],
        "fine_zone_max_x_m": fine_zone[1],
        "fine_zone_min_z_m": fine_zone[2],
        "fine_zone_max_z_m": fine_zone[3],
        "fine_particle_count": particle_counts["fine"],
        "coarse_particle_count": particle_counts["coarse"],
        "particle_count": len(particles),
    }


def fine_zone_bounds(
    anchor: AnchorAssembly,
    margin: float,
    pull_distance: float,
    pull_angle_deg: float,
) -> Tuple[float, float, float, float]:
    components = [anchor.base, *anchor.links]
    min_x = math.inf
    max_x = -math.inf
    min_z = math.inf
    max_z = -math.inf
    for component in components:
        axis_aligned_half_extents = np.abs(component.rel_rotation) @ component.half_extents
        center = component.rel_position
        min_x = min(min_x, float(center[0] - axis_aligned_half_extents[0]))
        max_x = max(max_x, float(center[0] + axis_aligned_half_extents[0]))
        min_z = min(min_z, float(center[2] - axis_aligned_half_extents[2]))
        max_z = max(max_z, float(center[2] + axis_aligned_half_extents[2]))

    angle = math.radians(pull_angle_deg)
    pull_offset = np.array([math.cos(angle) * pull_distance, math.sin(angle) * pull_distance])
    return (
        min(min_x, min_x + pull_offset[0]) - margin,
        max(max_x, max_x + pull_offset[0]) + margin,
        min(min_z, min_z + pull_offset[1]) - margin,
        max(max_z, max_z + pull_offset[1]) + margin,
    )


def estimate_soil_particle_upper_bound(
    anchor: AnchorAssembly,
    particle_radius: float,
    lateral_margin: float,
    bottom_margin: float,
    *,
    soil_mode: str = "adaptive",
    coarse_particle_radius: Optional[float] = None,
    fine_zone_margin: float = DEFAULT_FINE_ZONE_MARGIN_IN * INCH_TO_M,
    pull_distance: float = 0.0,
    pull_angle_deg: float = 0.0,
) -> int:
    base = anchor.base
    soil_bottom = anchor.base_bottom_y - bottom_margin
    soil_half_x = max(base.half_extents[0] + lateral_margin, 0.14)
    soil_half_z = max(base.half_extents[2] + lateral_margin, 0.14)
    fine_radius = max(1e-9, particle_radius)
    coarse_radius = max(fine_radius, coarse_particle_radius or fine_radius)
    use_adaptive = soil_mode == "adaptive" and coarse_radius > fine_radius * 1.01
    fine_zone = fine_zone_bounds(
        anchor=anchor,
        margin=max(0.0, fine_zone_margin),
        pull_distance=max(0.0, pull_distance),
        pull_angle_deg=pull_angle_deg,
    )

    def axis_count(lower: float, upper: float, spacing: float) -> int:
        if upper < lower:
            return 0
        return int(math.floor((upper - lower) / spacing)) + 1

    def grid_count(radius: float, bounds: Optional[Tuple[float, float, float, float]] = None) -> int:
        spacing = max(1e-9, 2.08 * radius)
        if bounds is None:
            lower_x = -soil_half_x + radius
            upper_x = soil_half_x - radius
            lower_z = -soil_half_z + radius
            upper_z = soil_half_z - radius
        else:
            lower_x = max(-soil_half_x + radius, bounds[0])
            upper_x = min(soil_half_x - radius, bounds[1])
            lower_z = max(-soil_half_z + radius, bounds[2])
            upper_z = min(soil_half_z - radius, bounds[3])
        x_count = axis_count(lower_x, upper_x, spacing)
        y_count = axis_count(soil_bottom + radius, anchor.surface_y - radius, spacing)
        z_count = axis_count(lower_z, upper_z, spacing)
        return max(0, x_count * y_count * z_count)

    if not use_adaptive:
        return grid_count(fine_radius)

    fine_count = grid_count(fine_radius, fine_zone)
    expanded_fine_zone = (
        fine_zone[0] - coarse_radius - fine_radius,
        fine_zone[1] + coarse_radius + fine_radius,
        fine_zone[2] - coarse_radius - fine_radius,
        fine_zone[3] + coarse_radius + fine_radius,
    )
    full_coarse_count = grid_count(coarse_radius)
    skipped_coarse_count = grid_count(coarse_radius, expanded_fine_zone)
    return max(0, fine_count + full_coarse_count - skipped_coarse_count)


def estimate_full_soil_particle_upper_bound(
    anchor: AnchorAssembly,
    particle_radius: float,
    lateral_margin: float,
    bottom_margin: float,
) -> int:
    base = anchor.base
    soil_bottom = anchor.base_bottom_y - bottom_margin
    soil_half_x = max(base.half_extents[0] + lateral_margin, 0.14)
    soil_half_z = max(base.half_extents[2] + lateral_margin, 0.14)
    spacing = max(1e-9, 2.08 * particle_radius)

    def axis_count(lower: float, upper: float) -> int:
        if upper < lower:
            return 0
        return int(math.floor((upper - lower) / spacing)) + 1

    x_count = axis_count(-soil_half_x + particle_radius, soil_half_x - particle_radius)
    y_count = axis_count(soil_bottom + particle_radius, anchor.surface_y - particle_radius)
    z_count = axis_count(-soil_half_z + particle_radius, soil_half_z - particle_radius)
    return max(0, x_count * y_count * z_count)


def estimate_particle_count_for_settings(anchor: AnchorAssembly, settings: SimulationParameters) -> int:
    return estimate_soil_particle_upper_bound(
        anchor=anchor,
        particle_radius=settings.particle_radius_in * INCH_TO_M,
        lateral_margin=settings.soil_lateral_margin_in * INCH_TO_M,
        bottom_margin=settings.soil_bottom_margin_in * INCH_TO_M,
        soil_mode=settings.soil_mode,
        coarse_particle_radius=settings.coarse_particle_radius_in * INCH_TO_M,
        fine_zone_margin=settings.fine_zone_margin_in * INCH_TO_M,
        pull_distance=settings.pull_distance_in * INCH_TO_M,
        pull_angle_deg=settings.pull_angle_deg,
    )


def rotate_vector(quaternion_xyzw: Iterable[float], vector: np.ndarray) -> np.ndarray:
    rotation = Rotation.from_quat(np.asarray(quaternion_xyzw, dtype=float))
    return rotation.apply(vector)


def base_state(body_id: int) -> Tuple[np.ndarray, np.ndarray]:
    position, quaternion = p.getBasePositionAndOrientation(body_id)
    return np.asarray(position, dtype=float), np.asarray(quaternion, dtype=float)


def get_pull_point_world(body_id: int, pull_point_local: np.ndarray) -> np.ndarray:
    position, quaternion = base_state(body_id)
    return position + rotate_vector(quaternion, pull_point_local)


def get_pull_height_limits(anchor: AnchorAssembly) -> Tuple[float, float]:
    min_height_in = 6.0
    max_height_in = max(min_height_in, (anchor.rod_tip_y - anchor.base_bottom_y) / INCH_TO_M)
    return min_height_in, max_height_in


def clamp_pull_height(anchor: AnchorAssembly, value_in: float) -> float:
    min_height, max_height = get_pull_height_limits(anchor)
    return float(np.clip(value_in, min_height, max_height))


def settings_from_args(args: argparse.Namespace, anchor: AnchorAssembly) -> SimulationParameters:
    settings = SimulationParameters(
        pull_distance_in=args.pull_distance_in,
        pull_speed_in_per_s=args.pull_speed_in_per_s,
        object_mass_lb=args.object_mass_lb,
        particle_radius_in=args.particle_radius_in,
        particle_density=args.particle_density,
        soil_lateral_margin_in=args.soil_lateral_margin_in,
        soil_bottom_margin_in=args.soil_bottom_margin_in,
        settle_time_s=args.settle_time_s,
        time_step=args.time_step,
        random_seed=args.random_seed,
        capture_every=args.capture_every,
        pull_force_lbf=args.pull_force_lbf,
        pull_height_in=args.pull_height_in,
        pull_angle_deg=args.pull_angle_deg,
        simulation_speed=args.simulation_speed,
        soil_mode=args.soil_mode,
        coarse_particle_radius_in=args.coarse_particle_radius_in,
        fine_zone_margin_in=args.fine_zone_margin_in,
        max_soil_particles=args.max_soil_particles,
        max_playback_memory_mb=args.max_playback_memory_mb,
    )
    return replace(settings, pull_height_in=clamp_pull_height(anchor, settings.pull_height_in))


def solve_simulation_worker(
    anchor: AnchorAssembly,
    settings: SimulationParameters,
    result_queue: mp.Queue,
    stop_event: mp.Event,
    pause_event: mp.Event,
) -> None:
    """Run PyBullet in a separate process so Tk remains responsive."""
    engine: Optional[SimulationEngine] = None
    try:
        engine = SimulationEngine(anchor, connection_mode=p.DIRECT)
        engine.reset(settings)
        result_queue.put(
            {
                "type": "started",
                "status": engine.current_status(settings),
                "particle_count": len(engine.particles),
            }
        )

        last_progress_time = time.perf_counter()
        while not engine.done and not stop_event.is_set():
            if pause_event.is_set():
                time.sleep(0.03)
                continue

            row = engine.step(settings)
            now = time.perf_counter()
            if now - last_progress_time >= 0.18 or engine.done:
                result_queue.put(
                    {
                        "type": "progress",
                        "row": row,
                        "status": engine.current_status(settings),
                        "row_count": len(engine.history),
                    }
                )
                last_progress_time = now

        if stop_event.is_set():
            result_queue.put({"type": "cancelled"})
            return

        completed_run = {
            "settings": settings,
            "history": engine.history,
            "summary": engine.summary(settings),
            "geometry_metadata": engine.geometry_metadata(settings),
            "snapshots": engine.snapshots,
            "soil_info": engine.soil_info,
            "anchor_mass_kg": engine.anchor_mass_kg,
            "peak_force_n": engine.peak_force_n,
            "extracted_time_s": engine.extracted_time_s,
            "status": engine.current_status(settings),
        }
        with tempfile.NamedTemporaryFile(prefix="pole_pullout_run_", suffix=".pkl", delete=False) as handle:
            pickle.dump(completed_run, handle, protocol=pickle.HIGHEST_PROTOCOL)
            result_path = handle.name
        result_queue.put({"type": "complete", "result_path": result_path})
    except Exception as exc:  # pragma: no cover - exercised through GUI runtime
        result_queue.put(
            {
                "type": "error",
                "message": str(exc),
                "traceback": traceback.format_exc(),
            }
        )
    finally:
        if engine is not None:
            engine.disconnect()


class SimulationEngine:
    def __init__(self, anchor: AnchorAssembly, connection_mode: int = p.DIRECT) -> None:
        self.anchor = anchor
        self.client = p.connect(connection_mode)
        if self.client < 0:
            raise RuntimeError("Failed to connect to PyBullet.")

        self.anchor_body: Optional[int] = None
        self.anchor_mass_kg = 0.0
        self.particles: List[int] = []
        self.soil_info: Dict[str, float] = {}
        self.pull_marker_body: Optional[int] = None
        self.target_marker_body: Optional[int] = None

        self.history: List[Dict[str, float]] = []
        self.sim_time_s = 0.0
        self.commanded_displacement_m = 0.0
        self.peak_force_n = 0.0
        self.extracted_time_s: Optional[float] = None
        self.done = False
        self.settings: Optional[SimulationParameters] = None
        self.required_clearance_m = anchor.surface_y - anchor.base_bottom_y
        self.base_start_position = np.zeros(3)
        self.base_start_quaternion = np.array([0.0, 0.0, 0.0, 1.0], dtype=float)
        self.last_force_world = np.zeros(3)
        self.last_target_world = np.zeros(3)
        self.snapshot_stride = 1
        self.step_index = 0
        self.snapshots: List[PlaybackSnapshot] = []
        self.recent_reaction_forces: List[np.ndarray] = []
        self.reaction_smoothing_steps = 12
        self.force_measurement_substeps = 1

    def disconnect(self) -> None:
        if p.isConnected(self.client):
            p.disconnect(self.client)

    def _configure_world(self, settings: SimulationParameters) -> None:
        p.resetSimulation()
        p.setGravity(0.0, -9.81, 0.0)
        p.setTimeStep(settings.time_step)
        p.setPhysicsEngineParameter(
            fixedTimeStep=settings.time_step,
            numSolverIterations=140,
            numSubSteps=2,
            enableConeFriction=1,
        )

    def _create_pull_markers(self) -> None:
        actual_visual = p.createVisualShape(
            p.GEOM_SPHERE,
            radius=0.006,
            rgbaColor=[0.90, 0.20, 0.15, 1.0],
        )
        target_visual = p.createVisualShape(
            p.GEOM_SPHERE,
            radius=0.006,
            rgbaColor=[0.10, 0.55, 0.90, 1.0],
        )
        self.pull_marker_body = p.createMultiBody(
            baseMass=0.0,
            baseCollisionShapeIndex=-1,
            baseVisualShapeIndex=actual_visual,
            basePosition=[0.0, 0.0, 0.0],
        )
        self.target_marker_body = p.createMultiBody(
            baseMass=0.0,
            baseCollisionShapeIndex=-1,
            baseVisualShapeIndex=target_visual,
            basePosition=[0.0, 0.0, 0.0],
        )

    def _hold_anchor_during_settle(self, settings: SimulationParameters) -> None:
        settle_steps = int(settings.settle_time_s / settings.time_step)
        base_position, base_quaternion = base_state(self.anchor_body)
        for _ in range(settle_steps):
            p.resetBasePositionAndOrientation(
                self.anchor_body,
                base_position.tolist(),
                base_quaternion.tolist(),
            )
            p.resetBaseVelocity(self.anchor_body, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0])
            p.stepSimulation()

    def _memory_limited_snapshot_stride(self, settings: SimulationParameters, particle_count: int) -> int:
        pull_speed_mps = settings.pull_speed_in_per_s * INCH_TO_M
        if pull_speed_mps <= 1e-9:
            return 1

        max_time_s = settings.pull_distance_in * INCH_TO_M / pull_speed_mps
        steps_per_engine_step = max(1, self.force_measurement_substeps)
        expected_engine_steps = max(
            1,
            int(math.ceil(max_time_s / (settings.time_step * steps_per_engine_step))),
        )
        bytes_per_snapshot = max(1, particle_count) * 3 * np.dtype(np.float32).itemsize
        memory_budget_bytes = max(16.0, settings.max_playback_memory_mb) * 1024.0 * 1024.0
        max_snapshots = max(2, int(memory_budget_bytes // max(1, bytes_per_snapshot)))
        return max(1, int(math.ceil(expected_engine_steps / max_snapshots)))

    def reset(self, settings: SimulationParameters) -> None:
        if settings.particle_radius_in <= 0.0:
            raise ValueError("Particle radius must be positive.")
        settings = replace(
            settings,
            pull_height_in=clamp_pull_height(self.anchor, settings.pull_height_in),
            coarse_particle_radius_in=max(settings.particle_radius_in, settings.coarse_particle_radius_in),
            fine_zone_margin_in=max(0.0, settings.fine_zone_margin_in),
            max_soil_particles=max(0, settings.max_soil_particles),
            max_playback_memory_mb=max(16.0, settings.max_playback_memory_mb),
        )
        estimated_particle_upper_bound = estimate_particle_count_for_settings(self.anchor, settings)
        estimated_full_particle_upper_bound = estimate_full_soil_particle_upper_bound(
            anchor=self.anchor,
            particle_radius=settings.particle_radius_in * INCH_TO_M,
            lateral_margin=settings.soil_lateral_margin_in * INCH_TO_M,
            bottom_margin=settings.soil_bottom_margin_in * INCH_TO_M,
        )
        if settings.max_soil_particles > 0 and estimated_particle_upper_bound > settings.max_soil_particles:
            raise ValueError(
                "The requested soil model exceeds the configured particle cap. "
                f"It would create about {estimated_particle_upper_bound:,} particles; "
                f"increase --max-soil-particles above that value or set it to 0 for no cap."
            )
        if estimated_particle_upper_bound > LARGE_SOIL_WARNING_PARTICLES:
            print(
                "Large soil model warning: about "
                f"{estimated_particle_upper_bound:,} particles will be created. "
                "This may take a long time and use substantial RAM.",
                flush=True,
            )

        self.settings = settings
        self._configure_world(settings)

        self.anchor_body, self.anchor_mass_kg = build_anchor(
            self.anchor,
            total_mass_kg=pounds_to_kg(settings.object_mass_lb),
        )
        rng = np.random.default_rng(settings.random_seed)
        self.particles, self.soil_info = create_soil_and_container(
            anchor=self.anchor,
            particle_radius=settings.particle_radius_in * INCH_TO_M,
            particle_density=settings.particle_density,
            lateral_margin=settings.soil_lateral_margin_in * INCH_TO_M,
            bottom_margin=settings.soil_bottom_margin_in * INCH_TO_M,
            rng=rng,
            soil_mode=settings.soil_mode,
            coarse_particle_radius=settings.coarse_particle_radius_in * INCH_TO_M,
            fine_zone_margin=settings.fine_zone_margin_in * INCH_TO_M,
            pull_distance=settings.pull_distance_in * INCH_TO_M,
            pull_angle_deg=settings.pull_angle_deg,
        )
        self.soil_info["estimated_particle_upper_bound"] = estimated_particle_upper_bound
        self.soil_info["estimated_full_fine_particle_upper_bound"] = estimated_full_particle_upper_bound
        self._create_pull_markers()
        self._hold_anchor_during_settle(settings)

        self.history = []
        self.sim_time_s = 0.0
        self.commanded_displacement_m = 0.0
        self.peak_force_n = 0.0
        self.extracted_time_s = None
        self.done = False
        self.step_index = 0
        self.force_measurement_substeps = max(1, min(6, int(round(0.0125 / settings.time_step))))
        base_snapshot_stride = max(1, int(round(settings.capture_every / self.force_measurement_substeps)))
        self.snapshot_stride = max(
            base_snapshot_stride,
            self._memory_limited_snapshot_stride(settings, len(self.particles)),
        )
        self.base_start_position, self.base_start_quaternion = base_state(self.anchor_body)
        self.last_force_world = np.zeros(3)
        self.last_target_world = self.neutral_pull_origin(settings)
        self.snapshots = []
        self.recent_reaction_forces = []
        self.reaction_smoothing_steps = max(
            1,
            int(round(0.05 / (settings.time_step * self.force_measurement_substeps))),
        )
        self.refresh_markers(settings)
        self.capture_snapshot(settings)

    def pull_point_local(self, settings: SimulationParameters) -> np.ndarray:
        return np.array(
            [0.0, self.anchor.base_bottom_y + settings.pull_height_in * INCH_TO_M, 0.0],
            dtype=float,
        )

    def pull_direction_world(self, settings: SimulationParameters) -> np.ndarray:
        # Pull direction stays parallel to the ground plane; the angle is an azimuth
        # about the vertical axis, with 0 deg pulling along +X.
        angle = math.radians(settings.pull_angle_deg)
        return normalize(np.array([math.cos(angle), 0.0, math.sin(angle)], dtype=float))

    def neutral_pull_origin(self, settings: SimulationParameters) -> np.ndarray:
        local_pull = self.pull_point_local(settings)
        return self.base_start_position + rotate_vector(self.base_start_quaternion, local_pull)

    def current_pull_point_world(self, settings: SimulationParameters) -> np.ndarray:
        return get_pull_point_world(self.anchor_body, self.pull_point_local(settings))

    def refresh_markers(self, settings: SimulationParameters) -> None:
        if self.pull_marker_body is None or self.target_marker_body is None:
            return
        current_pull_point = self.current_pull_point_world(settings)
        target_world = self.current_target_world(settings)
        p.resetBasePositionAndOrientation(
            self.pull_marker_body,
            current_pull_point.tolist(),
            [0.0, 0.0, 0.0, 1.0],
        )
        p.resetBasePositionAndOrientation(
            self.target_marker_body,
            target_world.tolist(),
            [0.0, 0.0, 0.0, 1.0],
        )

    def current_target_world(self, settings: SimulationParameters) -> np.ndarray:
        return self.neutral_pull_origin(settings) + self.pull_direction_world(settings) * self.commanded_displacement_m

    def current_status(self, settings: Optional[SimulationParameters] = None) -> Dict[str, float]:
        settings = settings or self.settings
        if settings is None:
            raise RuntimeError("Simulation settings are not available yet.")

        base_position, _ = base_state(self.anchor_body)
        direction = self.pull_direction_world(settings)
        pull_point_world = self.current_pull_point_world(settings)
        neutral_origin = self.neutral_pull_origin(settings)
        displacement_along_dir = self.commanded_displacement_m
        vertical_displacement = pull_point_world[1] - neutral_origin[1]
        force_total_n = abs(float(np.dot(self.last_force_world, direction)))
        vertical_force_n = abs(float(self.last_force_world[1]))

        return {
            "time_s": self.sim_time_s,
            "force_total_n": force_total_n,
            "force_total_lbf": force_total_n * LBF_PER_N,
            "force_vertical_lbf": vertical_force_n * LBF_PER_N,
            "displacement_along_dir_in": displacement_along_dir / INCH_TO_M,
            "vertical_displacement_in": vertical_displacement / INCH_TO_M,
            "commanded_displacement_in": self.commanded_displacement_m / INCH_TO_M,
            "base_bottom_y_m": base_position[1] + self.anchor.base_bottom_y,
            "pull_point_y_m": pull_point_world[1],
            "particle_count": len(self.particles),
        }

    def measure_anchor_reaction_force_world(self) -> np.ndarray:
        reaction_force = np.zeros(3, dtype=float)
        soil_particle_ids = set(self.particles)
        for contact in p.getContactPoints(bodyA=self.anchor_body):
            other_body = contact[2]
            if other_body not in soil_particle_ids:
                continue
            normal = np.asarray(contact[7], dtype=float)
            normal_force = float(contact[9])
            lateral_1_force = float(contact[10])
            lateral_1_dir = np.asarray(contact[11], dtype=float)
            lateral_2_force = float(contact[12])
            lateral_2_dir = np.asarray(contact[13], dtype=float)
            reaction_force += (
                normal * normal_force
                + lateral_1_dir * lateral_1_force
                + lateral_2_dir * lateral_2_force
            )
        return reaction_force

    def smoothed_reaction_force_world(self, raw_force_world: np.ndarray) -> np.ndarray:
        self.recent_reaction_forces.append(raw_force_world.astype(float))
        if len(self.recent_reaction_forces) > self.reaction_smoothing_steps:
            self.recent_reaction_forces.pop(0)
        return np.mean(self.recent_reaction_forces, axis=0)

    def capture_snapshot(self, settings: Optional[SimulationParameters] = None) -> None:
        settings = settings or self.settings
        if settings is None:
            raise RuntimeError("Simulation settings are not available yet.")

        anchor_position, anchor_orientation = base_state(self.anchor_body)
        particle_positions = np.asarray(
            [p.getBasePositionAndOrientation(body_id)[0] for body_id in self.particles],
            dtype=np.float32,
        )
        self.snapshots.append(
            PlaybackSnapshot(
                time_s=self.sim_time_s,
                commanded_displacement_m=self.commanded_displacement_m,
                anchor_position=anchor_position.astype(np.float32),
                anchor_orientation=anchor_orientation.astype(np.float32),
                particle_positions=particle_positions,
                force_world=self.last_force_world.astype(np.float32),
            )
        )

    def load_snapshot(self, index: int, settings: Optional[SimulationParameters] = None) -> None:
        settings = settings or self.settings
        if settings is None:
            raise RuntimeError("Simulation settings are not available yet.")
        if not self.snapshots:
            return

        snapshot = self.snapshots[int(np.clip(index, 0, len(self.snapshots) - 1))]
        p.resetBasePositionAndOrientation(
            self.anchor_body,
            snapshot.anchor_position.tolist(),
            snapshot.anchor_orientation.tolist(),
        )
        p.resetBaseVelocity(self.anchor_body, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0])

        for body_id, position in zip(self.particles, snapshot.particle_positions):
            p.resetBasePositionAndOrientation(body_id, position.tolist(), [0.0, 0.0, 0.0, 1.0])
            p.resetBaseVelocity(body_id, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0])

        self.sim_time_s = float(snapshot.time_s)
        self.commanded_displacement_m = float(snapshot.commanded_displacement_m)
        self.last_force_world = snapshot.force_world.astype(float)
        self.last_target_world = self.current_target_world(settings)
        self.refresh_markers(settings)

    def step(self, settings: Optional[SimulationParameters] = None) -> Dict[str, float]:
        settings = settings or self.settings
        if settings is None:
            raise RuntimeError("Simulation settings are not available yet.")

        settings = replace(settings, pull_height_in=clamp_pull_height(self.anchor, settings.pull_height_in))
        self.settings = settings

        pull_distance_m = settings.pull_distance_in * INCH_TO_M
        pull_speed_mps = settings.pull_speed_in_per_s * INCH_TO_M
        max_time_s = pull_distance_m / pull_speed_mps if pull_speed_mps > 1e-9 else 0.0

        if self.done:
            self.refresh_markers(settings)
            return self.current_status(settings)

        local_pull_point = self.pull_point_local(settings)
        direction = self.pull_direction_world(settings)
        force_samples: List[np.ndarray] = []
        base_position_after_step = None
        base_orientation_after_step = None

        for _ in range(self.force_measurement_substeps):
            next_time_s = min(self.sim_time_s + settings.time_step, max_time_s)
            self.commanded_displacement_m = min(next_time_s * pull_speed_mps, pull_distance_m)
            target_pull_point_world = self.current_target_world(settings)
            _, current_orientation = base_state(self.anchor_body)
            target_base_position = target_pull_point_world - rotate_vector(current_orientation, local_pull_point)
            p.resetBasePositionAndOrientation(
                self.anchor_body,
                target_base_position.tolist(),
                current_orientation.tolist(),
            )
            p.resetBaseVelocity(self.anchor_body, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0])
            p.stepSimulation()
            force_samples.append(self.measure_anchor_reaction_force_world())
            base_position_after_step, base_orientation_after_step = base_state(self.anchor_body)
            self.sim_time_s = next_time_s
            if pull_speed_mps <= 1e-9 or self.sim_time_s >= max_time_s:
                break

        if base_position_after_step is None or base_orientation_after_step is None:
            base_position_after_step, base_orientation_after_step = base_state(self.anchor_body)

        target_pull_point_world = self.current_target_world(settings)
        final_base_position = target_pull_point_world - rotate_vector(base_orientation_after_step, local_pull_point)
        p.resetBasePositionAndOrientation(
            self.anchor_body,
            final_base_position.tolist(),
            base_orientation_after_step.tolist(),
        )
        p.resetBaseVelocity(self.anchor_body, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0])
        base_position_after_step, base_orientation_after_step = base_state(self.anchor_body)

        averaged_force_world = (
            np.mean(np.asarray(force_samples, dtype=float), axis=0)
            if force_samples
            else np.zeros(3, dtype=float)
        )
        reaction_force_world = self.smoothed_reaction_force_world(averaged_force_world)
        self.last_force_world = reaction_force_world
        self.last_target_world = self.current_target_world(settings)

        pull_point_world = get_pull_point_world(self.anchor_body, local_pull_point)
        displacement_along_dir = self.commanded_displacement_m
        neutral_origin = self.neutral_pull_origin(settings)
        vertical_displacement = pull_point_world[1] - neutral_origin[1]
        measured_pull_force_n = abs(float(np.dot(reaction_force_world, direction)))
        measured_vertical_force_n = abs(float(reaction_force_world[1]))
        self.peak_force_n = max(self.peak_force_n, measured_pull_force_n)

        base_bottom_world = (
            base_position_after_step
            + rotate_vector(base_orientation_after_step, np.array([0.0, self.anchor.base_bottom_y, 0.0]))
        )[1]
        if (
            self.extracted_time_s is None
            and base_bottom_world >= self.anchor.surface_y
            and self.commanded_displacement_m >= self.required_clearance_m
        ):
            self.extracted_time_s = self.sim_time_s

        row = {
            "time_s": self.sim_time_s,
            "commanded_displacement_in": self.commanded_displacement_m / INCH_TO_M,
            "measured_displacement_in": displacement_along_dir / INCH_TO_M,
            "displacement_along_dir_in": displacement_along_dir / INCH_TO_M,
            "vertical_displacement_in": vertical_displacement / INCH_TO_M,
            "measured_pull_force_n": measured_pull_force_n,
            "measured_pull_force_lbf": measured_pull_force_n * LBF_PER_N,
            "measured_vertical_force_lbf": measured_vertical_force_n * LBF_PER_N,
            "reaction_force_x_n": reaction_force_world[0],
            "reaction_force_y_n": reaction_force_world[1],
            "reaction_force_z_n": reaction_force_world[2],
            "target_x_m": pull_point_world[0],
            "target_y_m": pull_point_world[1],
            "target_z_m": pull_point_world[2],
            "pull_point_x_m": pull_point_world[0],
            "pull_point_y_m": pull_point_world[1],
            "pull_point_z_m": pull_point_world[2],
            "base_bottom_y_m": base_bottom_world,
            "pull_angle_deg": settings.pull_angle_deg,
            "pull_height_in": settings.pull_height_in,
            "force_stop_threshold_lbf": settings.pull_force_lbf,
            "grain_size_in": settings.particle_radius_in,
        }
        self.history.append(row)
        self.refresh_markers(settings)
        self.step_index += 1
        if self.step_index % self.snapshot_stride == 0:
            self.capture_snapshot(settings)

        if (
            pull_speed_mps <= 1e-9
            or self.sim_time_s >= max_time_s
            or measured_pull_force_n >= settings.pull_force_lbf * N_PER_LBF
        ):
            self.done = True
            if not self.snapshots or self.snapshots[-1].time_s != self.sim_time_s:
                self.capture_snapshot(settings)

        return row

    def geometry_metadata(self, settings: Optional[SimulationParameters] = None) -> Dict[str, object]:
        settings = settings or self.settings
        if settings is None:
            raise RuntimeError("Simulation settings are not available yet.")

        return {
            "surface_y_m": self.anchor.surface_y,
            "base_bottom_y_m": self.anchor.base_bottom_y,
            "rod_tip_y_m": self.anchor.rod_tip_y,
            "object_mass_lb": settings.object_mass_lb,
            "object_mass_kg": pounds_to_kg(settings.object_mass_lb),
            "pull_height_in": settings.pull_height_in,
            "pull_angle_deg": settings.pull_angle_deg,
            "soil_mode": settings.soil_mode,
            "particle_radius_in": settings.particle_radius_in,
            "coarse_particle_radius_in": settings.coarse_particle_radius_in,
            "fine_zone_margin_in": settings.fine_zone_margin_in,
            "base_size_in": (self.anchor.base.dims / INCH_TO_M).tolist(),
            "anchor_to_particle_diameter_ratio": self.anchor.base.dims[0] / max(
                2.0 * settings.particle_radius_in * INCH_TO_M,
                1e-9,
            ),
            "particle_count": len(self.particles),
            **self.soil_info,
        }

    def summary(self, settings: Optional[SimulationParameters] = None) -> Dict[str, object]:
        settings = settings or self.settings
        if settings is None:
            raise RuntimeError("Simulation settings are not available yet.")

        return {
            "pull_distance_in": settings.pull_distance_in,
            "pull_speed_in_per_s": settings.pull_speed_in_per_s,
            "object_mass_lb": settings.object_mass_lb,
            "force_stop_threshold_lbf": settings.pull_force_lbf,
            "pull_height_in": settings.pull_height_in,
            "pull_angle_deg": settings.pull_angle_deg,
            "soil_mode": settings.soil_mode,
            "particle_radius_in": settings.particle_radius_in,
            "coarse_particle_radius_in": settings.coarse_particle_radius_in,
            "fine_zone_margin_in": settings.fine_zone_margin_in,
            "peak_force_lbf": self.peak_force_n * LBF_PER_N,
            "peak_force_n": self.peak_force_n,
            "anchor_mass_kg": self.anchor_mass_kg,
            "fully_extracted_at_s": self.extracted_time_s,
            "anchor_to_particle_diameter_ratio": self.anchor.base.dims[0] / max(
                2.0 * settings.particle_radius_in * INCH_TO_M,
                1e-9,
            ),
            "particle_count": len(self.particles),
        }

    def render(self, width: int = 900, height: int = 520) -> np.ndarray:
        target = np.array([0.0, 0.06, 0.0], dtype=float)
        distance = max(0.42, 2.0 * max(self.soil_info.get("soil_half_x", 0.15), self.soil_info.get("soil_height", 0.2)))
        view = p.computeViewMatrixFromYawPitchRoll(
            cameraTargetPosition=target.tolist(),
            distance=distance,
            yaw=35.0,
            pitch=-28.0,
            roll=0.0,
            upAxisIndex=1,
        )
        projection = p.computeProjectionMatrixFOV(
            fov=52.0,
            aspect=width / height,
            nearVal=0.01,
            farVal=3.0,
        )
        _, _, rgba, _, _ = p.getCameraImage(
            width=width,
            height=height,
            viewMatrix=view,
            projectionMatrix=projection,
            renderer=p.ER_TINY_RENDERER,
        )
        image = np.reshape(rgba, (height, width, 4)).astype(np.uint8)
        return image[:, :, :3]


def export_results(
    output_dir: Path,
    rows: List[Dict[str, float]],
    summary: Dict[str, object],
    geometry_metadata: Dict[str, object],
    *,
    include_plot: bool = True,
) -> Dict[str, Path]:
    output_dir.mkdir(parents=True, exist_ok=True)

    csv_path = output_dir / "force_displacement.csv"
    summary_path = output_dir / "summary.json"
    geometry_path = output_dir / "geometry_metadata.json"
    plot_path = output_dir / "force_displacement.png"

    if rows:
        with csv_path.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
            writer.writeheader()
            writer.writerows(rows)

    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")
    geometry_path.write_text(json.dumps(geometry_metadata, indent=2), encoding="utf-8")

    if include_plot and rows:
        x_values = [row["measured_displacement_in"] for row in rows]
        y_values = [row["measured_pull_force_lbf"] for row in rows]
        plt.figure(figsize=(8, 5))
        plt.plot(x_values, y_values, color="#0f4c81", linewidth=2.2)
        plt.xlabel("Measured pull-point displacement [in]")
        plt.ylabel("Measured pull force [lbf]")
        plt.title("Pullout response (displacement-controlled)")
        plt.grid(True, alpha=0.30)
        plt.tight_layout()
        plt.savefig(plot_path, dpi=220)
        plt.close()

    return {
        "csv": csv_path,
        "summary": summary_path,
        "geometry": geometry_path,
        "plot": plot_path,
    }


def run_batch(anchor: AnchorAssembly, args: argparse.Namespace) -> None:
    output_dir = args.output_dir
    settings = settings_from_args(args, anchor)
    engine = SimulationEngine(anchor, connection_mode=p.DIRECT)

    try:
        engine.reset(settings)
        video_path = output_dir / "pullout.mp4"
        writer = None
        if not args.no_video and cv2 is not None:
            writer = cv2.VideoWriter(
                str(video_path),
                cv2.VideoWriter_fourcc(*"mp4v"),
                max(1.0, 1.0 / (settings.time_step * max(settings.capture_every, 1))),
                (900, 520),
            )

        step_index = 0
        while not engine.done:
            engine.step(settings)
            if writer is not None and (step_index % max(settings.capture_every, 1) == 0):
                frame = engine.render(width=900, height=520)
                writer.write(cv2.cvtColor(frame, cv2.COLOR_RGB2BGR))
            step_index += 1

        if writer is not None:
            writer.release()

        outputs = export_results(
            output_dir=output_dir,
            rows=engine.history,
            summary=engine.summary(settings),
            geometry_metadata=engine.geometry_metadata(settings),
            include_plot=not args.no_plot,
        )

        print("Anchor mass [kg]:", round(engine.anchor_mass_kg, 3))
        print("Soil particles:", len(engine.particles))
        print("Outputs:")
        for name, path in outputs.items():
            if path.exists():
                print(f"  {name}: {path}")
        if writer is not None and video_path.exists():
            print(f"  video: {video_path}")
    finally:
        engine.disconnect()


class InteractivePulloutApp:
    def __init__(self, anchor: AnchorAssembly, args: argparse.Namespace) -> None:
        if tk is None or ttk is None or Image is None or ImageTk is None or FigureCanvasTkAgg is None:
            raise RuntimeError(
                "Interactive mode requires tkinter, Pillow, and matplotlib Tk bindings."
            )

        self.anchor = anchor
        self.args = args
        self.output_root = args.output_dir
        self.engine = SimulationEngine(anchor, connection_mode=p.DIRECT)
        self.solve_running = False
        self.solve_paused = False
        self.playback_running = False
        self.playback_index = 0
        self.pending_reset = False
        self._playback_budget = 0.0
        self._last_tick_time = time.perf_counter()
        self._last_view_refresh = 0.0
        self._last_plot_refresh = 0.0
        self._viewport_photo = None
        self.playback_fps = 15.0
        self.solve_process: Optional[mp.Process] = None
        self.solve_queue: Optional[mp.Queue] = None
        self.solve_stop_event: Optional[mp.Event] = None
        self.solve_pause_event: Optional[mp.Event] = None
        self.solve_status: Optional[Dict[str, float]] = None
        self.solve_progress_rows: List[Dict[str, float]] = []

        self.settings = settings_from_args(args, anchor)
        min_height, max_height = get_pull_height_limits(anchor)

        self.root = tk.Tk()
        self.root.title("Pole Pullout Simulator")
        self.root.geometry("1480x920")
        self.root.minsize(1280, 820)
        self.root.configure(bg="#eef2f7")
        self.root.protocol("WM_DELETE_WINDOW", self.close)

        self.play_button_var = tk.StringVar(value="Play")
        self.status_var = tk.StringVar(value="Ready")
        self.message_var = tk.StringVar(value="Adjust controls, then press Run Simulation.")
        self.preview_note_var = tk.StringVar(
            value="Run the simulation in fast mode. Playback becomes available after the solve completes."
        )
        self.time_var = tk.StringVar(value="0.00 s")
        self.force_var = tk.StringVar(value="0.0 lbf")
        self.vertical_force_var = tk.StringVar(value="0.0 lbf vertical")
        self.displacement_var = tk.StringVar(value="0.00 in")
        self.command_var = tk.StringVar(value="0.00 in")
        self.mass_var = tk.StringVar(value=f"{self.settings.object_mass_lb:.1f} lb")
        self.particles_var = tk.StringVar(value="0")
        self.export_var = tk.StringVar(value="No export yet")

        self.pull_distance_var = tk.DoubleVar(value=self.settings.pull_distance_in)
        self.pull_rate_var = tk.DoubleVar(value=self.settings.pull_speed_in_per_s)
        self.playback_speed_var = tk.DoubleVar(value=self.settings.simulation_speed)
        self.object_mass_var = tk.DoubleVar(value=self.settings.object_mass_lb)
        self.pull_force_var = tk.DoubleVar(value=self.settings.pull_force_lbf)
        self.pull_height_var = tk.DoubleVar(value=self.settings.pull_height_in)
        self.pull_angle_var = tk.DoubleVar(value=self.settings.pull_angle_deg)
        self.grain_size_var = tk.DoubleVar(value=self.settings.particle_radius_in)
        self.pull_distance_text = tk.StringVar()
        self.pull_rate_text = tk.StringVar()
        self.playback_speed_text = tk.StringVar()
        self.object_mass_text = tk.StringVar()
        self.pull_force_text = tk.StringVar()
        self.pull_height_text = tk.StringVar()
        self.pull_angle_text = tk.StringVar()
        self.grain_size_text = tk.StringVar()
        self.height_limits = (min_height, max_height)

        self._configure_style()
        self._build_ui()
        self._register_traces()
        self._refresh_control_text()
        self.reset_simulation()

        self.root.after(30, self._tick)

    def _configure_style(self) -> None:
        style = ttk.Style()
        if "clam" in style.theme_names():
            style.theme_use("clam")

        style.configure("App.TFrame", background="#eef2f7")
        style.configure("Panel.TFrame", background="#ffffff")
        style.configure("Card.TFrame", background="#ffffff")
        style.configure("Header.TLabel", background="#eef2f7", foreground="#0f172a", font=("Segoe UI Semibold", 22))
        style.configure("Subhead.TLabel", background="#eef2f7", foreground="#475569", font=("Segoe UI", 10))
        style.configure("PanelTitle.TLabel", background="#ffffff", foreground="#0f172a", font=("Segoe UI Semibold", 11))
        style.configure("ValueTitle.TLabel", background="#ffffff", foreground="#64748b", font=("Segoe UI", 9))
        style.configure("ValueNumber.TLabel", background="#ffffff", foreground="#0f172a", font=("Segoe UI Semibold", 14))
        style.configure("Body.TLabel", background="#ffffff", foreground="#334155", font=("Segoe UI", 10))
        style.configure("Hint.TLabel", background="#ffffff", foreground="#64748b", font=("Segoe UI", 9))
        style.configure("Accent.TButton", background="#0f766e", foreground="#ffffff", padding=(10, 8), font=("Segoe UI Semibold", 10))
        style.map("Accent.TButton", background=[("active", "#0b5d57")])
        style.configure("Neutral.TButton", background="#e2e8f0", foreground="#0f172a", padding=(10, 8), font=("Segoe UI Semibold", 10))
        style.map("Neutral.TButton", background=[("active", "#cbd5e1")])
        style.configure("Danger.TButton", background="#b91c1c", foreground="#ffffff", padding=(10, 8), font=("Segoe UI Semibold", 10))
        style.map("Danger.TButton", background=[("active", "#991b1b")])
        style.configure("Section.TLabelframe", background="#ffffff", borderwidth=1, relief="solid")
        style.configure("Section.TLabelframe.Label", background="#ffffff", foreground="#0f172a", font=("Segoe UI Semibold", 10))

    def _build_ui(self) -> None:
        outer = ttk.Frame(self.root, style="App.TFrame", padding=18)
        outer.pack(fill="both", expand=True)
        outer.columnconfigure(0, weight=0)
        outer.columnconfigure(1, weight=1)
        outer.rowconfigure(1, weight=1)

        header = ttk.Frame(outer, style="App.TFrame")
        header.grid(row=0, column=0, columnspan=2, sticky="ew", pady=(0, 14))
        ttk.Label(header, text="Pole Pullout Simulator", style="Header.TLabel").pack(anchor="w")
        ttk.Label(
            header,
            text="The pull path is horizontal and parallel to the ground, applied from the rod centerline at the selected height.",
            style="Subhead.TLabel",
        ).pack(anchor="w", pady=(4, 0))

        control_shell = ttk.Frame(outer, style="App.TFrame")
        control_shell.grid(row=1, column=0, sticky="nsw", padx=(0, 16))
        control_shell.rowconfigure(0, weight=1)
        control_shell.columnconfigure(0, weight=1)

        self.control_canvas = tk.Canvas(
            control_shell,
            background="#eef2f7",
            highlightthickness=0,
            width=372,
        )
        self.control_scrollbar = ttk.Scrollbar(
            control_shell,
            orient="vertical",
            command=self.control_canvas.yview,
        )
        self.control_canvas.configure(yscrollcommand=self.control_scrollbar.set)
        self.control_canvas.grid(row=0, column=0, sticky="ns")
        self.control_scrollbar.grid(row=0, column=1, sticky="ns")

        control_panel = ttk.Frame(self.control_canvas, style="Panel.TFrame", padding=16)
        self.control_window = self.control_canvas.create_window((0, 0), window=control_panel, anchor="nw")
        control_panel.bind("<Configure>", self._on_control_panel_configure)
        self.control_canvas.bind("<Configure>", self._on_control_canvas_configure)
        self.control_canvas.bind("<Enter>", self._bind_control_mousewheel)
        self.control_canvas.bind("<Leave>", self._unbind_control_mousewheel)

        content = ttk.Frame(outer, style="App.TFrame")
        content.grid(row=1, column=1, sticky="nsew")
        content.columnconfigure(0, weight=1)
        content.rowconfigure(0, weight=3)
        content.rowconfigure(1, weight=2)

        viewport_panel = ttk.Frame(content, style="Panel.TFrame", padding=12)
        viewport_panel.grid(row=0, column=0, sticky="nsew")
        viewport_panel.columnconfigure(0, weight=1)
        viewport_panel.rowconfigure(1, weight=1)
        viewport_header = ttk.Frame(viewport_panel, style="Panel.TFrame")
        viewport_header.grid(row=0, column=0, sticky="ew")
        viewport_header.columnconfigure(1, weight=1)
        ttk.Label(viewport_header, text="Viewport / Playback", style="PanelTitle.TLabel").grid(
            row=0, column=0, sticky="w"
        )
        ttk.Label(
            viewport_header,
            textvariable=self.preview_note_var,
            style="Hint.TLabel",
            wraplength=760,
            justify="right",
        ).grid(row=0, column=1, sticky="e", padx=(12, 0))
        self.viewport_label = ttk.Label(viewport_panel, style="Body.TLabel", anchor="center")
        self.viewport_label.grid(row=1, column=0, sticky="nsew", pady=(10, 0))

        plot_panel = ttk.Frame(content, style="Panel.TFrame", padding=12)
        plot_panel.grid(row=1, column=0, sticky="nsew", pady=(14, 0))
        plot_panel.columnconfigure(0, weight=1)
        plot_panel.rowconfigure(1, weight=1)

        ttk.Label(plot_panel, text="Force vs. Displacement", style="PanelTitle.TLabel").grid(
            row=0, column=0, sticky="w", pady=(0, 8)
        )
        self.figure = Figure(figsize=(7.2, 3.6), dpi=100)
        self.figure.patch.set_facecolor("#ffffff")
        self.axes = self.figure.add_subplot(111)
        self.axes.set_facecolor("#f8fafc")
        self.axes.grid(True, alpha=0.30)
        self.axes.set_xlabel("Measured displacement [in]")
        self.axes.set_ylabel("Measured pull force [lbf]")
        (self.plot_line,) = self.axes.plot([], [], color="#0f4c81", linewidth=2.2)
        self.plot_canvas = FigureCanvasTkAgg(self.figure, master=plot_panel)
        self.plot_canvas.get_tk_widget().grid(row=1, column=0, sticky="nsew")

        self._build_status_cards(control_panel)
        self._build_button_bar(control_panel)
        self._build_slider_sections(control_panel)

    def _on_control_panel_configure(self, _event=None) -> None:
        self.control_canvas.configure(scrollregion=self.control_canvas.bbox("all"))

    def _on_control_canvas_configure(self, event) -> None:
        self.control_canvas.itemconfigure(self.control_window, width=event.width)

    def _on_control_mousewheel(self, event) -> None:
        delta = event.delta
        if delta == 0:
            return
        self.control_canvas.yview_scroll(int(-delta / 120), "units")

    def _bind_control_mousewheel(self, _event=None) -> None:
        self.control_canvas.bind_all("<MouseWheel>", self._on_control_mousewheel)

    def _unbind_control_mousewheel(self, _event=None) -> None:
        self.control_canvas.unbind_all("<MouseWheel>")

    def _build_status_cards(self, parent: ttk.Frame) -> None:
        status_frame = ttk.Frame(parent, style="Panel.TFrame")
        status_frame.pack(fill="x")
        ttk.Label(status_frame, text="Live Status", style="PanelTitle.TLabel").pack(anchor="w")

        cards = ttk.Frame(status_frame, style="Panel.TFrame")
        cards.pack(fill="x", pady=(10, 0))
        for label_text, value_var in (
            ("State", self.status_var),
            ("Simulation time", self.time_var),
            ("Pull force", self.force_var),
            ("Vertical force", self.vertical_force_var),
            ("Measured displacement", self.displacement_var),
            ("Commanded displacement", self.command_var),
            ("Object mass", self.mass_var),
            ("Soil particles", self.particles_var),
        ):
            card = ttk.Frame(cards, style="Card.TFrame", padding=10)
            card.pack(fill="x", pady=4)
            ttk.Label(card, text=label_text, style="ValueTitle.TLabel").pack(anchor="w")
            ttk.Label(card, textvariable=value_var, style="ValueNumber.TLabel").pack(anchor="w", pady=(3, 0))

        msg_card = ttk.Frame(cards, style="Card.TFrame", padding=10)
        msg_card.pack(fill="x", pady=4)
        ttk.Label(msg_card, text="Notes", style="ValueTitle.TLabel").pack(anchor="w")
        ttk.Label(msg_card, textvariable=self.message_var, style="Body.TLabel", wraplength=300).pack(anchor="w", pady=(4, 0))
        ttk.Label(msg_card, textvariable=self.export_var, style="Hint.TLabel", wraplength=300).pack(anchor="w", pady=(6, 0))

    def _build_button_bar(self, parent: ttk.Frame) -> None:
        row = ttk.Frame(parent, style="Panel.TFrame")
        row.pack(fill="x", pady=(14, 10))

        ttk.Button(
            row,
            textvariable=self.play_button_var,
            style="Accent.TButton",
            command=self.toggle_play_pause,
        ).pack(side="left", fill="x", expand=True)
        ttk.Button(
            row,
            text="Reset Simulation",
            style="Neutral.TButton",
            command=self.reset_simulation,
        ).pack(side="left", fill="x", expand=True, padx=8)
        ttk.Button(
            row,
            text="Export Results",
            style="Neutral.TButton",
            command=self.export_results,
        ).pack(side="left", fill="x", expand=True)

    def _build_slider_sections(self, parent: ttk.Frame) -> None:
        playback = ttk.Labelframe(parent, text="Simulation", style="Section.TLabelframe", padding=12)
        playback.pack(fill="x", pady=(0, 10))
        self._add_slider(
            playback,
            "Solve Speed",
            self.playback_speed_var,
            0.25,
            6.0,
            self.playback_speed_text,
            lambda v: f"{v:.2f}x",
            live=True,
            decimals=2,
        )
        self._add_slider(
            playback,
            "Pull Rate",
            self.pull_rate_var,
            0.1,
            4.0,
            self.pull_rate_text,
            lambda v: f"{v:.2f} in/s",
            live=True,
            decimals=2,
        )
        self._add_slider(
            playback,
            "Pull Distance",
            self.pull_distance_var,
            0.5,
            12.0,
            self.pull_distance_text,
            lambda v: f"{v:.2f} in",
            live=True,
            decimals=2,
        )
        self._add_slider(
            playback,
            "Object Mass",
            self.object_mass_var,
            1.0,
            100.0,
            self.object_mass_text,
            lambda v: f"{v:.1f} lb",
            live=False,
            decimals=2,
            entry_min=0.1,
            entry_max=250.0,
        )

        load_path = ttk.Labelframe(parent, text="Load Path", style="Section.TLabelframe", padding=12)
        load_path.pack(fill="x", pady=(0, 10))
        self._add_slider(
            load_path,
            "Force Stop Threshold",
            self.pull_force_var,
            50.0,
            2000.0,
            self.pull_force_text,
            lambda v: f"{v:.0f} lbf",
            live=True,
            decimals=1,
            entry_min=10.0,
            entry_max=5000.0,
        )
        self._add_slider(
            load_path,
            "Pull Height on Rod Centerline",
            self.pull_height_var,
            self.height_limits[0],
            self.height_limits[1],
            self.pull_height_text,
            lambda v: f"{v:.2f} in",
            live=True,
            decimals=2,
            entry_min=self.height_limits[0],
            entry_max=self.height_limits[1],
        )
        self._add_slider(
            load_path,
            "Pull Direction in Ground Plane",
            self.pull_angle_var,
            -180.0,
            180.0,
            self.pull_angle_text,
            lambda v: f"{v:+.1f} deg",
            live=True,
            decimals=1,
            entry_min=-180.0,
            entry_max=180.0,
        )

        soil = ttk.Labelframe(parent, text="Soil", style="Section.TLabelframe", padding=12)
        soil.pack(fill="x")
        self._add_slider(
            soil,
            "Grain Size (Particle Radius)",
            self.grain_size_var,
            0.10,
            1.50,
            self.grain_size_text,
            lambda v: f"{v:.2f} in",
            live=False,
            decimals=3,
            entry_min=MIN_GRAIN_RADIUS_IN,
            entry_max=1.50,
        )
        ttk.Label(
            soil,
            text="Grain size and object mass rebuild the paused preview automatically. Very small grains can become expensive.",
            style="Hint.TLabel",
            wraplength=300,
        ).pack(anchor="w", pady=(6, 0))

    def _add_slider(
        self,
        parent: ttk.Labelframe,
        label: str,
        variable: tk.DoubleVar,
        start: float,
        end: float,
        text_variable: tk.StringVar,
        formatter,
        *,
        live: bool,
        decimals: int = 3,
        entry_min: Optional[float] = None,
        entry_max: Optional[float] = None,
    ) -> None:
        block = ttk.Frame(parent, style="Card.TFrame", padding=10)
        block.pack(fill="x", pady=4)

        top = ttk.Frame(block, style="Card.TFrame")
        top.pack(fill="x")
        ttk.Label(top, text=label, style="Body.TLabel").pack(side="left")
        ttk.Label(top, textvariable=text_variable, style="PanelTitle.TLabel").pack(side="right")

        scale = ttk.Scale(
            block,
            from_=start,
            to=end,
            variable=variable,
            command=lambda _=None, live=live: self._on_slider_change(live),
        )
        scale.pack(fill="x", pady=(8, 0))
        scale.bind("<ButtonRelease-1>", lambda _event, live=live: self._after_slider_release(live))

        entry_row = ttk.Frame(block, style="Card.TFrame")
        entry_row.pack(fill="x", pady=(8, 0))
        ttk.Label(entry_row, text="Exact value", style="Hint.TLabel").pack(side="left")
        entry_var = tk.StringVar(value=format_numeric_value(variable.get(), decimals))
        entry = ttk.Entry(entry_row, textvariable=entry_var, width=10, justify="right")
        entry.pack(side="right")

        min_value = start if entry_min is None else entry_min
        max_value = end if entry_max is None else entry_max

        def commit_entry(_event=None) -> None:
            try:
                entered_value = float(entry_var.get())
            except ValueError:
                entry_var.set(format_numeric_value(variable.get(), decimals))
                self.message_var.set(f"{label} expects a numeric value.")
                return

            clamped_value = float(np.clip(entered_value, min_value, max_value))
            variable.set(clamped_value)
            entry_var.set(format_numeric_value(clamped_value, decimals))
            self._on_slider_change(live)
            if not live:
                self._apply_nonlive_change()

        entry.bind("<Return>", commit_entry)
        entry.bind("<FocusOut>", commit_entry)

        def refresh_text(*_args) -> None:
            text_variable.set(formatter(variable.get()))
            if not entry.focus_get() == entry:
                entry_var.set(format_numeric_value(variable.get(), decimals))

        variable.trace_add("write", lambda *_args: refresh_text())
        refresh_text()

    def _register_traces(self) -> None:
        self.grain_size_var.trace_add("write", lambda *_: self._mark_pending_reset())
        self.object_mass_var.trace_add("write", lambda *_: self._mark_pending_reset())

    def _mark_pending_reset(self) -> None:
        self.pending_reset = True
        self.message_var.set(
            "Mass or grain size changed. The paused preview will rebuild on release; active runs rebuild on Reset or Run."
        )

    def _on_slider_change(self, live: bool) -> None:
        if self.solve_running:
            self.pending_reset = True
            self.message_var.set("Active run settings are locked. Reset after this solve to apply control changes.")
            return
        if live:
            self.message_var.set("Live controls update the preview. Reset applies any soil changes.")
        else:
            self._mark_pending_reset()

    def _after_slider_release(self, live: bool) -> None:
        if not live:
            self._apply_nonlive_change()

    def _apply_nonlive_change(self) -> None:
        if self.solve_running or self.playback_running:
            self._mark_pending_reset()
            return
        preview_particle_upper_bound = estimate_soil_particle_upper_bound(
            anchor=self.anchor,
            particle_radius=max(MIN_GRAIN_RADIUS_IN, self.grain_size_var.get()) * INCH_TO_M,
            lateral_margin=self.settings.soil_lateral_margin_in * INCH_TO_M,
            bottom_margin=self.settings.soil_bottom_margin_in * INCH_TO_M,
            soil_mode=self.settings.soil_mode,
            coarse_particle_radius=self.settings.coarse_particle_radius_in * INCH_TO_M,
            fine_zone_margin=self.settings.fine_zone_margin_in * INCH_TO_M,
            pull_distance=self.pull_distance_var.get() * INCH_TO_M,
            pull_angle_deg=self.pull_angle_var.get(),
        )
        if preview_particle_upper_bound > AUTO_PREVIEW_SOIL_PARTICLES:
            self.pending_reset = True
            self.message_var.set(
                "The new soil bed is too large for instant preview. Press Run or Reset to rebuild it with the new grain size."
            )
            return
        self.reset_simulation()

    def _refresh_control_text(self) -> None:
        self.pull_distance_text.set(f"{self.pull_distance_var.get():.2f} in")
        self.pull_rate_text.set(f"{self.pull_rate_var.get():.2f} in/s")
        self.playback_speed_text.set(f"{self.playback_speed_var.get():.2f}x")
        self.object_mass_text.set(f"{self.object_mass_var.get():.1f} lb")
        self.pull_force_text.set(f"{self.pull_force_var.get():.0f} lbf")
        self.pull_height_text.set(f"{self.pull_height_var.get():.2f} in")
        self.pull_angle_text.set(f"{self.pull_angle_var.get():+.1f} deg")
        self.grain_size_text.set(f"{self.grain_size_var.get():.2f} in")

    def current_settings(self) -> SimulationParameters:
        return replace(
            self.settings,
            pull_distance_in=max(0.1, self.pull_distance_var.get()),
            pull_speed_in_per_s=max(0.01, self.pull_rate_var.get()),
            object_mass_lb=max(0.1, self.object_mass_var.get()),
            particle_radius_in=max(MIN_GRAIN_RADIUS_IN, self.grain_size_var.get()),
            pull_force_lbf=max(10.0, self.pull_force_var.get()),
            pull_height_in=clamp_pull_height(self.anchor, self.pull_height_var.get()),
            pull_angle_deg=self.pull_angle_var.get(),
            simulation_speed=max(0.05, self.playback_speed_var.get()),
        )

    def playback_settings(self) -> SimulationParameters:
        return self.engine.settings or self.settings

    def _release_solver_handles(self) -> None:
        process = self.solve_process
        if process is not None and not process.is_alive():
            process.join(timeout=0.05)
        if self.solve_queue is not None:
            try:
                self.solve_queue.close()
            except (OSError, ValueError):
                pass
        self.solve_process = None
        self.solve_queue = None
        self.solve_stop_event = None
        self.solve_pause_event = None

    def _stop_background_solve(self, *, terminate: bool) -> None:
        if self.solve_stop_event is not None:
            self.solve_stop_event.set()
        if self.solve_pause_event is not None:
            self.solve_pause_event.clear()

        process = self.solve_process
        if process is not None and process.is_alive():
            process.join(timeout=0.35)
            if terminate and process.is_alive():
                process.terminate()
                process.join(timeout=1.0)

        self.solve_running = False
        self.solve_paused = False
        self._release_solver_handles()

    def _start_background_solve(self) -> None:
        if self.solve_running:
            return

        run_settings = self.current_settings()
        if run_settings.particle_radius_in <= 0.0:
            self.pending_reset = True
            self.status_var.set("Build Error")
            self.message_var.set("Particle radius must be positive.")
            self._set_play_button_text()
            return
        run_settings = replace(
            run_settings,
            coarse_particle_radius_in=max(run_settings.particle_radius_in, run_settings.coarse_particle_radius_in),
            fine_zone_margin_in=max(0.0, run_settings.fine_zone_margin_in),
            max_soil_particles=max(0, run_settings.max_soil_particles),
            max_playback_memory_mb=max(16.0, run_settings.max_playback_memory_mb),
        )
        estimated_particle_count = estimate_particle_count_for_settings(self.anchor, run_settings)
        if run_settings.max_soil_particles > 0 and estimated_particle_count > run_settings.max_soil_particles:
            self.pending_reset = True
            self.status_var.set("Build Error")
            self.message_var.set(
                f"Run would create about {estimated_particle_count:,} particles, above --max-soil-particles."
            )
            self._set_play_button_text()
            return

        self.settings = run_settings
        self.pending_reset = False
        self.playback_running = False
        self.playback_index = 0
        self.solve_status = {
            "time_s": 0.0,
            "force_total_lbf": 0.0,
            "force_vertical_lbf": 0.0,
            "displacement_along_dir_in": 0.0,
            "commanded_displacement_in": 0.0,
            "particle_count": float(estimated_particle_count),
        }
        self.solve_progress_rows = []

        context = mp.get_context("spawn")
        self.solve_queue = context.Queue()
        self.solve_stop_event = context.Event()
        self.solve_pause_event = context.Event()
        self.solve_process = context.Process(
            target=solve_simulation_worker,
            args=(
                self.anchor,
                run_settings,
                self.solve_queue,
                self.solve_stop_event,
                self.solve_pause_event,
            ),
            name="pole-pullout-solver",
            daemon=True,
        )
        try:
            self.solve_process.start()
        except Exception as exc:
            self._stop_background_solve(terminate=True)
            self.status_var.set("Start Error")
            self.message_var.set(f"Could not start background solver: {exc}")
            self._set_play_button_text()
            return

        self.solve_running = True
        self.solve_paused = False
        self._playback_budget = 0.0
        self._last_tick_time = time.perf_counter()
        self.status_var.set("Solving")
        self.preview_note_var.set(
            "Background solve in progress. The UI stays responsive; playback is loaded when the run finishes."
        )
        self.message_var.set("Solving in a separate process. Controls changed during this run apply after Reset.")
        self.export_var.set("No export yet")
        self._refresh_viewport(force=True)
        self._refresh_plot(force=True)
        self._set_play_button_text()

    def _pause_background_solve(self) -> None:
        if not self.solve_running or self.solve_pause_event is None:
            return
        self.solve_pause_event.set()
        self.solve_paused = True
        self.status_var.set("Solve Paused")
        self.preview_note_var.set("Background solve is paused. Resume to continue or Reset to cancel and rebuild.")
        self.message_var.set("Solve paused. The current run settings are preserved in the worker process.")
        self._set_play_button_text()

    def _resume_background_solve(self) -> None:
        if not self.solve_running or self.solve_pause_event is None:
            return
        self.solve_pause_event.clear()
        self.solve_paused = False
        self._last_tick_time = time.perf_counter()
        self.status_var.set("Solving")
        self.preview_note_var.set(
            "Background solve in progress. The UI stays responsive; playback is loaded when the run finishes."
        )
        self.message_var.set("Solve resumed.")
        self._set_play_button_text()

    def _load_completed_run(self, message: Dict[str, Any]) -> None:
        result_path = message.get("result_path")
        if result_path is not None:
            path = Path(result_path)
            try:
                with path.open("rb") as handle:
                    message = pickle.load(handle)
            finally:
                try:
                    path.unlink()
                except OSError:
                    pass

        run_settings = message["settings"]
        snapshots = message.get("snapshots", [])
        if snapshots:
            expected_particles = len(snapshots[0].particle_positions)
            if expected_particles != len(self.engine.particles):
                self.engine.reset(run_settings)

        self.settings = run_settings
        self.engine.settings = run_settings
        self.engine.history = message.get("history", [])
        self.engine.snapshots = snapshots
        self.engine.soil_info = message.get("soil_info", {})
        self.engine.anchor_mass_kg = float(message.get("anchor_mass_kg", self.engine.anchor_mass_kg))
        self.engine.peak_force_n = float(message.get("peak_force_n", 0.0))
        self.engine.extracted_time_s = message.get("extracted_time_s")
        self.engine.done = True
        self.engine.step_index = len(self.engine.history)
        self.solve_status = message.get("status")
        self.solve_progress_rows = self.engine.history

        self.solve_running = False
        self.solve_paused = False
        self.playback_running = False
        self.playback_index = 0
        self._playback_budget = 0.0
        self._release_solver_handles()

        if self.engine.snapshots:
            self.engine.load_snapshot(0, run_settings)
        self.preview_note_var.set("Solve complete. Use Play Playback to review the recorded run frame by frame.")
        self.message_var.set("Run complete. Playback and export are ready.")
        self._refresh_viewport(force=True)
        self._refresh_plot(force=True)
        self._set_play_button_text()

    def _handle_solver_message(self, message: Dict[str, Any]) -> None:
        message_type = message.get("type")
        if message_type == "started":
            self.solve_status = message.get("status")
            self.particles_var.set(f"{int(message.get('particle_count', 0))}")
            return

        if message_type == "progress":
            self.solve_status = message.get("status")
            row = message.get("row")
            if row is not None:
                self.solve_progress_rows.append(row)
            return

        if message_type == "complete":
            self._load_completed_run(message)
            return

        if message_type == "cancelled":
            self.solve_running = False
            self.solve_paused = False
            self._release_solver_handles()
            self.status_var.set("Paused")
            self.preview_note_var.set("Solve cancelled. Press Run Simulation to start again.")
            self.message_var.set("Background solve cancelled.")
            self._set_play_button_text()
            return

        if message_type == "error":
            self.solve_running = False
            self.solve_paused = False
            self._release_solver_handles()
            self.status_var.set("Solve Error")
            self.preview_note_var.set("The background solver failed. Check the terminal output for the traceback.")
            self.message_var.set(str(message.get("message", "Unknown solver error.")))
            traceback_text = message.get("traceback")
            if traceback_text:
                print(traceback_text)
            self._set_play_button_text()

    def _drain_solver_messages(self) -> None:
        result_queue = self.solve_queue
        if result_queue is None:
            return

        while True:
            try:
                message = result_queue.get_nowait()
            except Empty:
                break
            self._handle_solver_message(message)
            if self.solve_queue is None:
                return

        process = self.solve_process
        if self.solve_running and process is not None and not process.is_alive():
            time.sleep(0.02)
            while self.solve_queue is not None:
                try:
                    message = self.solve_queue.get_nowait()
                except Empty:
                    break
                self._handle_solver_message(message)
                if self.solve_queue is None:
                    return
            if not self.solve_running:
                return
            exitcode = process.exitcode
            self.solve_running = False
            self.solve_paused = False
            self._release_solver_handles()
            self.status_var.set("Solve Error")
            self.preview_note_var.set("The background solver exited before returning a completed run.")
            self.message_var.set(f"Background solver exited unexpectedly with code {exitcode}.")
            self._set_play_button_text()

    def _set_play_button_text(self) -> None:
        if self.solve_running:
            self.play_button_var.set("Resume Solve" if self.solve_paused else "Pause Solve")
        elif not self.engine.done:
            self.play_button_var.set("Run Simulation")
        elif self.playback_running:
            self.play_button_var.set("Pause Playback")
        else:
            self.play_button_var.set("Play Playback")

    def toggle_play_pause(self) -> None:
        if self.solve_running:
            if self.solve_paused:
                self._resume_background_solve()
            else:
                self._pause_background_solve()
            return

        if not self.engine.done:
            if self.pending_reset and not self.reset_simulation():
                return
            self._start_background_solve()
            return
        else:
            if not self.engine.snapshots:
                self.message_var.set("No playback snapshots are available for this run.")
                return
            self.solve_running = False
            if not self.playback_running and self.playback_index >= len(self.engine.snapshots) - 1:
                self.playback_index = 0
                self.engine.load_snapshot(self.playback_index, self.playback_settings())
            self.playback_running = not self.playback_running
            if self.playback_running:
                self._playback_budget = 0.0
                self.status_var.set("Playing Back")
                self.preview_note_var.set("Reviewing the recorded run. Press Pause Playback to stop on the current frame.")
                self.message_var.set("Playback running. Reset to rebuild and rerun with new soil settings.")
            else:
                self.status_var.set("Playback Paused")
                self.preview_note_var.set("Playback paused. Press Play Playback to continue or Reset to rerun.")
                self.message_var.set("Playback paused.")
        self._set_play_button_text()

    def _preview_settings_for(self, settings: SimulationParameters) -> Tuple[SimulationParameters, bool, int]:
        requested_count = estimate_particle_count_for_settings(self.anchor, settings)
        if requested_count <= AUTO_PREVIEW_SOIL_PARTICLES:
            return settings, False, requested_count

        preview_settings = replace(settings, max_soil_particles=0)
        for _ in range(24):
            preview_settings = replace(
                preview_settings,
                particle_radius_in=preview_settings.particle_radius_in * 1.25,
                coarse_particle_radius_in=max(
                    preview_settings.coarse_particle_radius_in,
                    preview_settings.particle_radius_in * 1.25,
                ),
            )
            preview_count = estimate_particle_count_for_settings(self.anchor, preview_settings)
            if preview_count <= AUTO_PREVIEW_SOIL_PARTICLES:
                return preview_settings, True, requested_count

        return preview_settings, True, requested_count

    def reset_simulation(self) -> bool:
        if self.solve_running or self.solve_process is not None:
            self._stop_background_solve(terminate=True)
        self.solve_running = False
        self.solve_paused = False
        self.playback_running = False
        self.playback_index = 0
        self.solve_status = None
        self.solve_progress_rows = []
        next_settings = self.current_settings()
        if next_settings.particle_radius_in <= 0.0:
            self.pending_reset = True
            self.status_var.set("Build Error")
            self.message_var.set("Particle radius must be positive.")
            self._set_play_button_text()
            return False
        next_settings = replace(
            next_settings,
            coarse_particle_radius_in=max(next_settings.particle_radius_in, next_settings.coarse_particle_radius_in),
            fine_zone_margin_in=max(0.0, next_settings.fine_zone_margin_in),
            max_soil_particles=max(0, next_settings.max_soil_particles),
            max_playback_memory_mb=max(16.0, next_settings.max_playback_memory_mb),
        )
        preview_settings, preview_is_coarsened, requested_particle_count = self._preview_settings_for(next_settings)
        try:
            self.engine.reset(preview_settings)
        except ValueError as exc:
            self.pending_reset = True
            self.status_var.set("Build Error")
            self.preview_note_var.set(
                "The requested soil bed could not be built for preview. Adjust particle settings or run batch mode."
            )
            self.message_var.set(str(exc))
            self._set_play_button_text()
            return False

        self.settings = next_settings
        self.pending_reset = False
        self._playback_budget = 0.0
        self._last_tick_time = time.perf_counter()
        self.status_var.set("Paused")
        self.preview_note_var.set(
            "Ready to solve. Press Run Simulation for a fast solve, then review the playback afterward."
        )
        if preview_is_coarsened:
            self.message_var.set(
                "Large run ready. The viewport preview uses coarser particles; Run Simulation uses "
                f"the requested settings and about {requested_particle_count:,} particles."
            )
        else:
            self.message_var.set("Simulation reset and paused at the beginning with the current settings.")
        self.export_var.set("No export yet")
        self._set_play_button_text()
        self.engine.load_snapshot(0, self.settings)
        self._update_status_text()
        self._refresh_viewport(force=True)
        self._refresh_plot(force=True)
        return True

    def _update_status_text(self) -> None:
        if self.solve_running and self.solve_status is not None:
            status_settings = self.settings
            status = self.solve_status
        else:
            status_settings = self.playback_settings() if self.engine.done else self.current_settings()
            status = self.engine.current_status(status_settings)
        self.time_var.set(f"{status['time_s']:.2f} s")
        self.force_var.set(f"{status['force_total_lbf']:.1f} lbf")
        self.vertical_force_var.set(f"{status['force_vertical_lbf']:.1f} lbf vertical")
        self.displacement_var.set(f"{status['displacement_along_dir_in']:.2f} in")
        self.command_var.set(f"{status['commanded_displacement_in']:.2f} in")
        self.mass_var.set(f"{status_settings.object_mass_lb:.1f} lb")
        self.particles_var.set(f"{int(status['particle_count'])}")

        if self.solve_running:
            self.status_var.set("Solve Paused" if self.solve_paused else "Solving")
        elif self.playback_running:
            self.status_var.set("Playing Back")
        elif self.engine.done:
            self.status_var.set("Playback Ready")
        else:
            self.status_var.set("Paused")

        self._set_play_button_text()

    def _refresh_plot(self, force: bool = False) -> None:
        now = time.perf_counter()
        if not force and now - self._last_plot_refresh < 0.18:
            return
        self._last_plot_refresh = now

        rows = self.solve_progress_rows if self.solve_running else self.engine.history
        if rows:
            x_values = [row["measured_displacement_in"] for row in rows]
            y_values = [row["measured_pull_force_lbf"] for row in rows]
            self.plot_line.set_data(x_values, y_values)
            self.axes.relim()
            self.axes.autoscale_view()
        else:
            self.plot_line.set_data([], [])
            self.axes.set_xlim(0.0, 1.0)
            self.axes.set_ylim(0.0, 1.0)

        self.plot_canvas.draw_idle()

    def _refresh_viewport(self, force: bool = False) -> None:
        now = time.perf_counter()
        if not force and now - self._last_view_refresh < 0.08:
            return
        if self.solve_running and not force:
            return
        self._last_view_refresh = now

        frame = self.engine.render(width=900, height=520)
        image = Image.fromarray(frame)
        resampling = getattr(Image, "Resampling", Image)
        image = image.resize((920, 520), resample=resampling.LANCZOS)
        self._viewport_photo = ImageTk.PhotoImage(image=image)
        self.viewport_label.configure(image=self._viewport_photo)

    def export_results(self) -> None:
        if self.solve_running:
            self.message_var.set("Wait for the background solve to finish, or reset to cancel it, before exporting.")
            return
        if not self.engine.history:
            self.message_var.set("No completed run is available to export yet.")
            return
        session_dir = self.output_root / f"interactive_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        outputs = export_results(
            output_dir=session_dir,
            rows=self.engine.history,
            summary=self.engine.summary(self.playback_settings()),
            geometry_metadata=self.engine.geometry_metadata(self.playback_settings()),
            include_plot=True,
        )
        self.export_var.set(f"Last export: {session_dir}")
        self.message_var.set("Current run exported.")
        if outputs["plot"].exists():
            self.message_var.set(f"Current run exported to {session_dir}.")

    def _tick(self) -> None:
        self._drain_solver_messages()
        now = time.perf_counter()
        elapsed = min(now - self._last_tick_time, 0.25)
        self._last_tick_time = now
        if self.solve_running:
            settings = self.settings
        else:
            settings = self.current_settings()
            self.settings = settings
            if abs(self.pull_height_var.get() - settings.pull_height_in) > 1e-9:
                self.pull_height_var.set(settings.pull_height_in)

        if self.playback_running:
            if self.engine.snapshots:
                self._playback_budget += elapsed * self.playback_fps
                frames_to_advance = max(1, int(self._playback_budget))
                self._playback_budget -= frames_to_advance
                self.playback_index = min(
                    self.playback_index + frames_to_advance,
                    len(self.engine.snapshots) - 1,
                )
                self.engine.load_snapshot(self.playback_index, self.playback_settings())
                if self.playback_index >= len(self.engine.snapshots) - 1:
                    self.playback_running = False
                    self.preview_note_var.set(
                        "Playback reached the end. Press Play Playback to replay from the start."
                    )
                    self.message_var.set("Playback complete.")
                    self._set_play_button_text()
        else:
            if not self.solve_running:
                self.engine.refresh_markers(settings)

        self._update_status_text()
        if not self.solve_running:
            self._refresh_viewport()
        self._refresh_plot()
        self.root.after(30, self._tick)

    def run(self) -> None:
        self.root.mainloop()

    def close(self) -> None:
        try:
            if self.solve_running or self.solve_process is not None:
                self._stop_background_solve(terminate=True)
            self.engine.disconnect()
        finally:
            self.root.destroy()


def launch_interactive_app(anchor: AnchorAssembly, args: argparse.Namespace) -> None:
    app = InteractivePulloutApp(anchor, args)
    app.run()


def main() -> None:
    mp.freeze_support()
    args = parse_args()
    anchor = load_anchor_assembly(args.step_file)
    if args.interactive:
        launch_interactive_app(anchor, args)
    else:
        run_batch(anchor, args)


if __name__ == "__main__":
    main()
