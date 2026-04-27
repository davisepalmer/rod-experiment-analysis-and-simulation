get_script_dir = function() {
  cmd_args = commandArgs(trailingOnly = FALSE)
  file_arg = "--file="
  script_path = grep(paste0("^", file_arg), cmd_args, value = TRUE)

  if (length(script_path) > 0) {
    resolved_script_path = sub(file_arg, "", script_path[1])

    if (nzchar(resolved_script_path) && resolved_script_path != "-") {
      return(dirname(normalizePath(resolved_script_path)))
    }
  }

  frame_files = vapply(
    sys.frames(),
    function(env) {
      if (!is.null(env$ofile)) env$ofile else NA_character_
    },
    character(1)
  )
  frame_files = frame_files[!is.na(frame_files)]

  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[length(frame_files)])))
  }

  normalizePath(getwd())
}

source(file.path(get_script_dir(), "loadRaw.R"), chdir = TRUE)

plot_font_family = "serif"
if (.Platform$OS.type == "windows") {
  windowsFonts(Times = windowsFont("Times New Roman"))
  plot_font_family = "Times"
}

normalize_shape_name = function(shape_name) {
  shape_lookup = c(
    circle = "Circle",
    ibeam = "IBeam",
    square = "Square"
  )

  shape_lookup[[tolower(shape_name)]]
}

trial_names = names(all_trials)

parse_trial_factors = function(trial_name) {
  prefix = sub("_T[0-9]+$", "", trial_name)
  moisture_condition = extract_condition_prefix(prefix)
  beam_dimension = normalize_shape_name(strip_condition_prefix(prefix))

  data.frame(
    trial = trial_name,
    condition = prefix,
    moisture_condition = moisture_condition,
    beam_dimension = beam_dimension,
    soil_moisture_level = condition_prefix_to_numeric(prefix),
    stringsAsFactors = FALSE
  )
}

trial_metadata = do.call(rbind, lapply(trial_names, parse_trial_factors))
trial_metadata$moisture_condition = factor(
  trial_metadata$moisture_condition,
  levels = condition_prefix_levels,
  labels = condition_display_labels[condition_prefix_levels]
)
trial_metadata$beam_dimension = factor(trial_metadata$beam_dimension, levels = c("Circle", "IBeam", "Square"))

model_data = do.call(rbind, lapply(trial_names, function(trial_name) {
  trial_df = all_trials[[trial_name]]
  meta = trial_metadata[trial_metadata$trial == trial_name, ][1, ]

  data.frame(
    trial = trial_name,
    condition = meta$condition,
    moisture_condition = meta$moisture_condition,
    soil_moisture_level = meta$soil_moisture_level,
    beam_dimension = meta$beam_dimension,
    displacement_in = trial_df$`displacement[in]`,
    force_lbf = trial_df$`force[lbf]`,
    stringsAsFactors = FALSE
  )
}))

model_data$trial = factor(model_data$trial)
model_data$moisture_condition = factor(model_data$moisture_condition, levels = levels(trial_metadata$moisture_condition))
model_data$beam_dimension = factor(model_data$beam_dimension, levels = levels(trial_metadata$beam_dimension))
model_data$disp_c = model_data$displacement_in - mean(model_data$displacement_in, na.rm = TRUE)
model_data$moist_c = model_data$soil_moisture_level - mean(model_data$soil_moisture_level, na.rm = TRUE)

eqn = log1p(force_lbf) ~
  beam_dimension +
  moist_c +
  disp_c + I(disp_c^2) + I(disp_c^3) +
  beam_dimension:moist_c +
  disp_c:beam_dimension +
  I(disp_c^2):beam_dimension +
  disp_c:moist_c +
  I(disp_c^2):moist_c

model = lm(eqn, data = model_data)

example_conditions = c("0uWaterSquare", "10uWaterCircle", "15uWaterIBeam")

trial_palette = c("red", "blue", "darkgreen", "orange", "purple")

old_par = par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(mfrow = c(1, length(example_conditions)), mar = c(4.5, 4.5, 3.5, 1.5), family = plot_font_family)

for (condition_prefix in example_conditions) {
  condition_trial_names = names(all_trials)[grepl(paste0("^", condition_prefix, "_T[0-9]+$"), names(all_trials))]
  if (length(condition_trial_names) == 0 && grepl("IBeam$", condition_prefix)) {
    alternate_prefix = sub("IBeam$", "Ibeam", condition_prefix)
    condition_trial_names = names(all_trials)[grepl(paste0("^", alternate_prefix, "_T[0-9]+$"), names(all_trials))]
  }
  condition_trial_names = condition_trial_names[order(as.numeric(sub(".*_T", "", condition_trial_names)))]

  condition_data = do.call(rbind, lapply(condition_trial_names, function(trial_name) {
    trial_df = all_trials[[trial_name]]
    data.frame(
      trial = trial_name,
      displacement_in = trial_df$`displacement[in]`,
      force_lbf = trial_df$`force[lbf]`
    )
  }))

  condition_data$trial = factor(condition_data$trial)
  point_colors = trial_palette[condition_data$trial]

  shape_name = normalize_shape_name(strip_condition_prefix(condition_prefix))
  moisture_value = condition_prefix_to_numeric(condition_prefix)
  panel_label = paste(condition_prefix_to_label(condition_prefix), shape_name)

  plot(
    condition_data$displacement_in,
    condition_data$force_lbf,
    col = point_colors,
    pch = 16,
    xlab = "Displacement (in)",
    ylab = "Force (lbf)",
    main = panel_label
  )

  displacement_grid = seq(
    min(condition_data$displacement_in, na.rm = TRUE),
    max(condition_data$displacement_in, na.rm = TRUE),
    length.out = 500
  )

  prediction_data = data.frame(
    beam_dimension = factor(shape_name, levels = levels(model_data$beam_dimension)),
    moist_c = moisture_value - mean(model_data$soil_moisture_level, na.rm = TRUE),
    disp_c = displacement_grid - mean(model_data$displacement_in, na.rm = TRUE)
  )

  predicted_log_force = predict(model, newdata = prediction_data)
  predicted_force = expm1(predicted_log_force)

  lines(displacement_grid, predicted_force, col = "black", lwd = 5)

  legend(
    "topleft",
    legend = levels(condition_data$trial),
    col = trial_palette[seq_along(levels(condition_data$trial))],
    pch = 16,
    bty = "n"
  )
}
