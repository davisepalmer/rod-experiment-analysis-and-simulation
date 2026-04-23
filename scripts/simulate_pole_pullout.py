#!/usr/bin/env python3
"""
Coarse 3D pullout simulation for the squareAndPole STEP assembly.

The script:
1. Parses the assembly STEP file to recover component placements.
2. Reads standalone component STEP files to recover bounding boxes.
3. Builds a simplified rigid multibody anchor in PyBullet.
4. Approximates soil as many spherical particles in a retaining box.
5. Pulls the anchor vertically from the nut elevation and records force
   versus displacement.

This is a pragmatic granular prototype, not a calibrated geotechnical model.
For higher-fidelity soil mechanics, use a DEM/FEM tool such as Project Chrono,
EDEM, Abaqus, or LS-DYNA.
"""

from __future__ import annotations

import argparse
import csv
import json
import math
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Tuple

import gmsh
import matplotlib.pyplot as plt
import numpy as np
import pybullet as p
from scipy.spatial.transform import Rotation

try:
    import cv2
except ImportError:  # pragma: no cover - optional runtime dependency
    cv2 = None


INCH_TO_M = 0.0254
LBF_PER_N = 0.2248089431
FLOAT_RE = re.compile(r"[-+]?(?:\d+\.\d*|\d+|\.\d+)(?:[Ee][-+]?\d+)?")


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
    pull_point_local: np.ndarray
    surface_y: float
    base_bottom_y: float
    rod_tip_y: float


def parse_args() -> argparse.Namespace:
    default_step = Path("poleGeometryStepFiles") / "squareAndPole.STEP"
    default_output = Path("outputs") / "pole_pullout_sim"

    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--step-file", type=Path, default=default_step)
    parser.add_argument("--output-dir", type=Path, default=default_output)
    parser.add_argument("--gui", action="store_true", help="Run with the PyBullet GUI.")
    parser.add_argument("--no-video", action="store_true", help="Skip MP4 export.")
    parser.add_argument("--no-plot", action="store_true", help="Skip force plot export.")
    parser.add_argument("--pull-distance-in", type=float, default=6.5)
    parser.add_argument("--pull-speed-in-per-s", type=float, default=1.0)
    parser.add_argument("--particle-radius-in", type=float, default=0.80)
    parser.add_argument("--particle-density", type=float, default=1700.0)
    parser.add_argument("--soil-lateral-margin-in", type=float, default=5.0)
    parser.add_argument("--soil-bottom-margin-in", type=float, default=2.5)
    parser.add_argument("--settle-time-s", type=float, default=0.5)
    parser.add_argument("--time-step", type=float, default=1.0 / 240.0)
    parser.add_argument("--random-seed", type=int, default=7)
    parser.add_argument("--capture-every", type=int, default=8)
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
        stem, suffix = instance_name.split("__instance_", maxsplit=1)
        base_name = f"{stem}{Path(instance_name).suffix}"
        return assembly_dir / base_name
    return assembly_dir / instance_name


def gmsh_bbox(step_path: Path) -> BoundingBox:
    gmsh.clear()
    gmsh.model.add(step_path.stem)
    gmsh.model.occ.importShapes(str(step_path))
    gmsh.model.occ.synchronize()
    bbox = gmsh.model.getBoundingBox(-1, -1)
    mins = np.array(bbox[:3], dtype=float)
    maxs = np.array(bbox[3:], dtype=float)
    # Gmsh's STEP importer exposes the geometry here in millimeters.
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

    base_name, _, base_bbox, base_transform_world, base_center_world, base_rotation_world = base_candidates[0]
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

    nut_positions = [component.rel_position for component in components if component.role == "nut"]
    if nut_positions:
        pull_point_local = np.mean(np.vstack(nut_positions), axis=0)
    else:
        pull_point_local = np.zeros(3)
    pull_point_local[1] = base_bottom_y + 17.5 * INCH_TO_M

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
        pull_point_local=pull_point_local,
        surface_y=surface_y,
        base_bottom_y=base_bottom_y,
        rod_tip_y=rod_tip_y,
    )


def volume_from_bbox(dims: np.ndarray) -> float:
    return float(np.prod(dims))


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


def build_anchor(anchor: AnchorAssembly) -> Tuple[int, float]:
    steel_density = 7850.0

    base = anchor.base
    base_mass = steel_density * volume_from_bbox(base.dims)
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

    total_mass = base_mass
    for component in anchor.links:
        mass = steel_density * volume_from_bbox(component.dims)
        total_mass += mass

        collision = p.createCollisionShape(p.GEOM_BOX, halfExtents=component.half_extents.tolist())
        color = [0.72, 0.72, 0.76, 1.0]
        if component.role == "rod":
            color = [0.55, 0.55, 0.58, 1.0]
        if component.role == "nut":
            color = [0.80, 0.74, 0.20, 1.0]
        visual = p.createVisualShape(
            p.GEOM_BOX,
            halfExtents=component.half_extents.tolist(),
            rgbaColor=color,
        )

        link_masses.append(mass)
        link_collision.append(collision)
        link_visual.append(visual)
        link_positions.append(component.rel_position.tolist())
        link_orientations.append(rotation_quat_xyzw(component.rel_rotation))
        link_inertial_positions.append([0.0, 0.0, 0.0])
        link_inertial_orientations.append([0.0, 0.0, 0.0, 1.0])
        link_parent_indices.append(0)
        link_joint_types.append(p.JOINT_FIXED)
        link_joint_axes.append([0.0, 0.0, 0.0])

    base_position = [0.0, 0.0, 0.0]
    anchor_body = p.createMultiBody(
        baseMass=base_mass,
        baseCollisionShapeIndex=base_collision,
        baseVisualShapeIndex=base_visual,
        basePosition=base_position,
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

    return anchor_body, total_mass


def create_soil_and_container(
    anchor: AnchorAssembly,
    particle_radius: float,
    particle_density: float,
    lateral_margin: float,
    bottom_margin: float,
    rng: np.random.Generator,
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
        [0.70, 0.70, 0.72, 0.25],
    )
    create_static_box(
        [wall_thickness, wall_half_y, soil_half_z + wall_thickness],
        [-soil_half_x - wall_thickness, wall_center_y, 0.0],
        [0.70, 0.70, 0.72, 0.25],
    )
    create_static_box(
        [soil_half_x + wall_thickness, wall_half_y, wall_thickness],
        [0.0, wall_center_y, soil_half_z + wall_thickness],
        [0.70, 0.70, 0.72, 0.25],
    )
    create_static_box(
        [soil_half_x + wall_thickness, wall_half_y, wall_thickness],
        [0.0, wall_center_y, -soil_half_z - wall_thickness],
        [0.70, 0.70, 0.72, 0.25],
    )

    sphere_collision = p.createCollisionShape(p.GEOM_SPHERE, radius=particle_radius)
    sphere_visual = p.createVisualShape(
        p.GEOM_SPHERE,
        radius=particle_radius,
        rgbaColor=[0.56, 0.42, 0.28, 1.0],
    )

    sphere_volume = 4.0 / 3.0 * math.pi * particle_radius**3
    sphere_mass = particle_density * sphere_volume

    spacing = 2.08 * particle_radius
    xs = np.arange(-soil_half_x + particle_radius, soil_half_x - particle_radius + 1e-9, spacing)
    ys = np.arange(soil_bottom + particle_radius, anchor.surface_y - particle_radius + 1e-9, spacing)
    zs = np.arange(-soil_half_z + particle_radius, soil_half_z - particle_radius + 1e-9, spacing)

    particles: List[int] = []
    for iy, y in enumerate(ys):
        x_offset = (iy % 2) * particle_radius
        for iz, z in enumerate(zs):
            z_offset = ((iy + iz) % 2) * particle_radius * 0.5
            for x in xs:
                position = np.array(
                    [
                        x + x_offset - 0.5 * particle_radius,
                        y,
                        z + z_offset,
                    ],
                    dtype=float,
                )
                position += rng.uniform(-0.03 * particle_radius, 0.03 * particle_radius, size=3)
                if position[1] > anchor.surface_y - 0.5 * particle_radius:
                    continue

                occupied = point_inside_component(position, anchor.base, padding=1.15 * particle_radius)
                if not occupied:
                    occupied = any(
                        point_inside_component(position, component, padding=0.9 * particle_radius)
                        for component in anchor.links
                    )
                if occupied:
                    continue

                body = p.createMultiBody(
                    baseMass=sphere_mass,
                    baseCollisionShapeIndex=sphere_collision,
                    baseVisualShapeIndex=sphere_visual,
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

    return particles, {
        "soil_bottom": soil_bottom,
        "soil_half_x": soil_half_x,
        "soil_half_z": soil_half_z,
        "soil_height": soil_height,
        "particle_count": len(particles),
    }


def rotate_vector(quaternion_xyzw: Iterable[float], vector: np.ndarray) -> np.ndarray:
    rotation = Rotation.from_quat(np.asarray(quaternion_xyzw, dtype=float))
    return rotation.apply(vector)


def base_state(body_id: int) -> Tuple[np.ndarray, np.ndarray]:
    position, quaternion = p.getBasePositionAndOrientation(body_id)
    return np.asarray(position, dtype=float), np.asarray(quaternion, dtype=float)


def get_pull_point_world(body_id: int, pull_point_local: np.ndarray) -> np.ndarray:
    position, quaternion = base_state(body_id)
    return position + rotate_vector(quaternion, pull_point_local)


def capture_frame(width: int = 1280, height: int = 720) -> np.ndarray:
    view = p.computeViewMatrix(
        cameraEyePosition=[0.38, 0.10, 0.30],
        cameraTargetPosition=[0.0, -0.03, 0.0],
        cameraUpVector=[0.0, 1.0, 0.0],
    )
    projection = p.computeProjectionMatrixFOV(
        fov=55.0,
        aspect=width / height,
        nearVal=0.01,
        farVal=2.0,
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


def simulate_pullout(
    anchor: AnchorAssembly,
    anchor_body: int,
    anchor_mass: float,
    args: argparse.Namespace,
    output_dir: Path,
) -> Dict[str, Path]:
    time_step = args.time_step
    settle_steps = int(args.settle_time_s / time_step)
    pull_distance = args.pull_distance_in * INCH_TO_M
    pull_speed = args.pull_speed_in_per_s * INCH_TO_M
    pull_duration = pull_distance / max(pull_speed, 1e-6)
    pull_steps = int(math.ceil(pull_duration / time_step))

    base_start, base_quat_start = base_state(anchor_body)

    for _ in range(settle_steps):
        p.resetBasePositionAndOrientation(anchor_body, base_start.tolist(), base_quat_start.tolist())
        p.resetBaseVelocity(anchor_body, [0.0, 0.0, 0.0], [0.0, 0.0, 0.0])
        p.stepSimulation()

    pull_point_start = get_pull_point_world(anchor_body, anchor.pull_point_local)

    k_vertical = 3200.0
    c_vertical = 260.0
    k_lateral = 2200.0
    c_lateral = 200.0
    torque_damping = 40.0
    max_force = 5500.0

    csv_path = output_dir / "force_displacement.csv"
    plot_path = output_dir / "force_displacement.png"
    video_path = output_dir / "pullout.mp4"

    frame_writer = None
    if not args.no_video and cv2 is not None:
        frame_writer = cv2.VideoWriter(
            str(video_path),
            cv2.VideoWriter_fourcc(*"mp4v"),
            max(1.0, 1.0 / (time_step * max(args.capture_every, 1))),
            (1280, 720),
        )

    rows = []
    peak_force_n = 0.0
    extracted_step = None
    required_clearance = anchor.surface_y - anchor.base_bottom_y

    for step in range(pull_steps):
        current_time = step * time_step
        commanded_displacement = min(current_time * pull_speed, pull_distance)
        target_y = pull_point_start[1] + commanded_displacement

        pull_point_world = get_pull_point_world(anchor_body, anchor.pull_point_local)
        linear_velocity, angular_velocity = p.getBaseVelocity(anchor_body)
        linear_velocity = np.asarray(linear_velocity, dtype=float)
        angular_velocity = np.asarray(angular_velocity, dtype=float)

        error = np.array(
            [
                -pull_point_world[0],
                target_y - pull_point_world[1],
                -pull_point_world[2],
            ],
            dtype=float,
        )
        force = np.array(
            [
                k_lateral * error[0] - c_lateral * linear_velocity[0],
                k_vertical * error[1] - c_vertical * linear_velocity[1],
                k_lateral * error[2] - c_lateral * linear_velocity[2],
            ],
            dtype=float,
        )
        force = np.clip(force, -max_force, max_force)
        p.applyExternalForce(anchor_body, -1, force.tolist(), pull_point_world.tolist(), p.WORLD_FRAME)
        p.applyExternalTorque(
            anchor_body,
            -1,
            (-torque_damping * angular_velocity).tolist(),
            p.WORLD_FRAME,
        )

        p.stepSimulation()

        base_position, _ = base_state(anchor_body)
        pull_point_world = get_pull_point_world(anchor_body, anchor.pull_point_local)
        displacement = pull_point_world[1] - pull_point_start[1]
        applied_force_n = float(np.dot(force, np.array([0.0, 1.0, 0.0])))
        peak_force_n = max(peak_force_n, applied_force_n)

        base_bottom_world = base_position[1] + anchor.base_bottom_y
        if (
            extracted_step is None
            and base_bottom_world >= anchor.surface_y
            and commanded_displacement >= required_clearance
        ):
            extracted_step = step

        rows.append(
            {
                "time_s": current_time,
                "pull_point_y_m": pull_point_world[1],
                "displacement_m": displacement,
                "displacement_in": displacement / INCH_TO_M,
                "applied_force_n": applied_force_n,
                "applied_force_lbf": applied_force_n * LBF_PER_N,
                "target_pull_y_m": target_y,
                "commanded_displacement_m": commanded_displacement,
                "commanded_displacement_in": commanded_displacement / INCH_TO_M,
                "base_center_y_m": base_position[1],
                "base_bottom_y_m": base_bottom_world,
                "base_top_y_m": base_position[1] + anchor.base.half_extents[1],
            }
        )

        if frame_writer is not None and (step % max(args.capture_every, 1) == 0):
            frame = capture_frame()
            frame_writer.write(cv2.cvtColor(frame, cv2.COLOR_RGB2BGR))

    if frame_writer is not None:
        frame_writer.release()

    with csv_path.open("w", newline="", encoding="utf-8") as handle:
        writer = csv.DictWriter(handle, fieldnames=list(rows[0].keys()))
        writer.writeheader()
        writer.writerows(rows)

    if not args.no_plot:
        displacements = [row["displacement_in"] for row in rows]
        forces = [row["applied_force_lbf"] for row in rows]
        plt.figure(figsize=(8, 5))
        plt.plot(displacements, forces, color="#1f5c99", linewidth=2.0)
        plt.xlabel("Pull point displacement [in]")
        plt.ylabel("Applied pull force [lbf]")
        plt.title("Coarse pullout response")
        plt.grid(True, alpha=0.3)
        plt.tight_layout()
        plt.savefig(plot_path, dpi=220)
        plt.close()

    summary = {
        "pull_distance_in": args.pull_distance_in,
        "pull_speed_in_per_s": args.pull_speed_in_per_s,
        "peak_force_lbf": peak_force_n * LBF_PER_N,
        "peak_force_n": peak_force_n,
        "anchor_mass_kg": anchor_mass,
        "fully_extracted_at_s": None if extracted_step is None else extracted_step * time_step,
    }
    summary_path = output_dir / "summary.json"
    summary_path.write_text(json.dumps(summary, indent=2), encoding="utf-8")

    return {
        "csv": csv_path,
        "plot": plot_path,
        "video": video_path,
        "summary": summary_path,
    }


def main() -> None:
    args = parse_args()
    output_dir = args.output_dir
    output_dir.mkdir(parents=True, exist_ok=True)

    anchor = load_anchor_assembly(args.step_file)

    connection_mode = p.GUI if args.gui else p.DIRECT
    client = p.connect(connection_mode)
    if client < 0:
        raise RuntimeError("Failed to connect to PyBullet.")

    try:
        p.resetSimulation()
        p.setGravity(0.0, -9.81, 0.0)
        p.setTimeStep(args.time_step)
        p.setPhysicsEngineParameter(
            fixedTimeStep=args.time_step,
            numSolverIterations=140,
            numSubSteps=2,
            enableConeFriction=1,
        )

        anchor_body, anchor_mass = build_anchor(anchor)
        rng = np.random.default_rng(args.random_seed)
        particles, soil_info = create_soil_and_container(
            anchor=anchor,
            particle_radius=args.particle_radius_in * INCH_TO_M,
            particle_density=args.particle_density,
            lateral_margin=args.soil_lateral_margin_in * INCH_TO_M,
            bottom_margin=args.soil_bottom_margin_in * INCH_TO_M,
            rng=rng,
        )

        metadata = {
            "surface_y_m": anchor.surface_y,
            "base_bottom_y_m": anchor.base_bottom_y,
            "rod_tip_y_m": anchor.rod_tip_y,
            "pull_point_local_m": anchor.pull_point_local.tolist(),
            "base_size_in": (anchor.base.dims / INCH_TO_M).tolist(),
            "particle_count": len(particles),
            **soil_info,
        }
        (output_dir / "geometry_metadata.json").write_text(
            json.dumps(metadata, indent=2),
            encoding="utf-8",
        )

        outputs = simulate_pullout(
            anchor=anchor,
            anchor_body=anchor_body,
            anchor_mass=anchor_mass,
            args=args,
            output_dir=output_dir,
        )

        print("Anchor mass [kg]:", round(anchor_mass, 3))
        print("Soil particles:", len(particles))
        print("Outputs:")
        for name, path in outputs.items():
            if path.exists():
                print(f"  {name}: {path}")
    finally:
        p.disconnect()


if __name__ == "__main__":
    main()
