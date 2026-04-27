# Saves standard lm diagnostic plots for the final reduced regression model.
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
plot_output_dir = file.path(project_root, "outputs", "reducedModelDiagnostics")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)

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

trial_names = names(all_trials)
trial_metadata = do.call(rbind, lapply(trial_names, parse_trial_factors))
trial_metadata$beam_dimension = factor(trial_metadata$beam_dimension, levels = c("Circle", "IBeam", "Square"))

model_data = do.call(rbind, lapply(trial_names, function(trial_name) {
  trial_df = all_trials[[trial_name]]
  meta = trial_metadata[trial_metadata$trial == trial_name, ][1, ]

  data.frame(
    trial = trial_name,
    soil_moisture_level = meta$soil_moisture_level,
    beam_dimension = meta$beam_dimension,
    displacement_in = trial_df$`displacement[in]`,
    force_lbf = trial_df$`force[lbf]`,
    stringsAsFactors = FALSE
  )
}))

model_data$beam_dimension = factor(model_data$beam_dimension, levels = levels(trial_metadata$beam_dimension))
model_data = add_reduced_equation_columns(model_data)

reduced_eqn = force_lbf ~ IB + SQ + M + D + D2 + SQxM + IBxD + SQxD + SQxD2 + MxD + MxD2
reduced_model = lm(reduced_eqn, data = model_data)

plot_names = c(
  "residuals_vs_fitted",
  "normal_qq",
  "scale_location",
  "residuals_vs_leverage"
)

for (i in seq_along(plot_names)) {
  output_file = file.path(plot_output_dir, paste0(i, "_", plot_names[i], ".png"))
  png(filename = output_file, width = 7, height = 5, units = "in", res = 300)
  old_par = par(no.readonly = TRUE)
  old_par$pin = NULL
  par(family = plot_font_family)
  plot(reduced_model, which = i)
  par(old_par)
  dev.off()
}

cat("Saved reduced model diagnostic plots to:\n")
cat(plot_output_dir, "\n", sep = "")
