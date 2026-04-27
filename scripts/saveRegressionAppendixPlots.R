# Saves one PNG per trial with the final reduced regression equation overlaid.
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

project_root = normalizePath(file.path(get_script_dir(), ".."))
plot_output_dir = file.path(project_root, "outputs", "regressionAppendixPlots")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)

normalize_shape_name = function(shape_name) {
  shape_lookup = c(
    circle = "Circle",
    ibeam = "IBeam",
    square = "Square"
  )

  shape_lookup[[tolower(shape_name)]]
}

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

add_reduced_equation_columns = function(data) {
  data$IB = as.integer(data$beam_dimension == "IBeam")
  data$SQ = as.integer(data$beam_dimension == "Square")
  data$M = data$soil_moisture_level
  data$D = data$displacement_in
  data$D2 = data$D^2
  data$SQxM = data$SQ * data$M
  data$IBxD = data$IB * data$D
  data$SQxD = data$SQ * data$D
  data$SQxD2 = data$SQ * data$D2
  data$MxD = data$M * data$D
  data$MxD2 = data$M * data$D2

  data
}

safe_filename = function(x) {
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

open_png_device = function(filename) {
  if (.Platform$OS.type == "windows") {
    png(filename = filename, width = 7, height = 5, units = "in", res = 300, type = "windows")
  } else if (capabilities("cairo")) {
    png(filename = filename, width = 7, height = 5, units = "in", res = 300, type = "cairo")
  } else {
    png(filename = filename, width = 2100, height = 1500, res = 300)
  }
}

plot_font_family = "serif"
if (.Platform$OS.type == "windows") {
  windowsFonts(TimesNewRoman = windowsFont("Times New Roman"))
  plot_font_family = "TimesNewRoman"
}

trial_names = names(all_trials)
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
model_data = add_reduced_equation_columns(model_data)

reduced_eqn = force_lbf ~ IB + SQ + M + D + D2 + SQxM + IBxD + SQxD + SQxD2 + MxD + MxD2
reduced_model = lm(reduced_eqn, data = model_data)

for (trial_name in trial_names) {
  trial_df = all_trials[[trial_name]]
  meta = trial_metadata[trial_metadata$trial == trial_name, ][1, ]

  x_range = range(trial_df$`displacement[in]`, na.rm = TRUE)
  displacement_grid = seq(x_range[1], x_range[2], length.out = 500)
  prediction_data = data.frame(
    beam_dimension = factor(meta$beam_dimension, levels = levels(model_data$beam_dimension)),
    soil_moisture_level = meta$soil_moisture_level,
    displacement_in = displacement_grid
  )
  prediction_data = add_reduced_equation_columns(prediction_data)
  regression_curve = predict(reduced_model, newdata = prediction_data)

  y_range = range(c(trial_df$`force[lbf]`, regression_curve), na.rm = TRUE)
  output_file = file.path(plot_output_dir, paste0(safe_filename(trial_name), ".png"))

  open_png_device(output_file)
  old_par = par(no.readonly = TRUE)
  old_par$pin = NULL
  par(family = plot_font_family, mar = c(4.5, 4.5, 3.5, 1))

  plot(
    trial_df$`displacement[in]`,
    trial_df$`force[lbf]`,
    type = "l",
    col = "steelblue4",
    lwd = 2,
    xlab = "Displacement (in)",
    ylab = "Force (lbf)",
    main = paste(condition_prefix_to_label(meta$condition), meta$beam_dimension, sub(".*_T", "Trial ", trial_name)),
    xlim = x_range,
    ylim = y_range
  )
  lines(displacement_grid, regression_curve, col = "black", lwd = 3)

  par(old_par)
  dev.off()
}

cat("Saved regression appendix plots to:\n")
cat(plot_output_dir, "\n", sep = "")
