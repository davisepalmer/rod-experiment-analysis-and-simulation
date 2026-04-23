# Summarizes peak force by moisture condition for each beam shape using trial-level data.
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

save_plots_as_png = FALSE
project_root = normalizePath(file.path(get_script_dir(), ".."))
plot_output_dir = file.path(project_root, "outputs", "plots")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)
plot_output_file = file.path(plot_output_dir, "peak_force_summary_plots.png")
moisture_levels = condition_prefix_levels
beam_levels = c("Circle", "IBeam", "Square")
moisture_colors = c(
  "0uWater" = "saddlebrown",
  "5uWater" = "blue",
  "10uWater" = "darkgreen",
  "15uWater" = "orange"
)

normalize_shape_name = function(shape_name) {
  shape_lookup = c(
    circle = "Circle",
    ibeam = "IBeam",
    square = "Square"
  )

  shape_lookup[[tolower(shape_name)]]
}

trial_summary = do.call(rbind, lapply(names(all_trials), function(trial_name) {
  prefix = sub("_T[0-9]+$", "", trial_name)
  moisture_condition = extract_condition_prefix(prefix)
  beam_dimension = normalize_shape_name(strip_condition_prefix(prefix))
  trial_df = all_trials[[trial_name]]

  data.frame(
    trial = trial_name,
    moisture_condition = moisture_condition,
    beam_dimension = beam_dimension,
    peak_force = max(trial_df$`force[lbf]`, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))

trial_summary$moisture_condition = factor(
  trial_summary$moisture_condition,
  levels = condition_prefix_levels,
  labels = condition_display_labels[condition_prefix_levels]
)
trial_summary$beam_dimension = factor(trial_summary$beam_dimension, levels = beam_levels)

if (save_plots_as_png) {
  png(filename = plot_output_file, width = 1500, height = 550, res = 150)
  on.exit(dev.off(), add = TRUE)
}

old_par = par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

for (beam_name in beam_levels) {
  beam_data = trial_summary[trial_summary$beam_dimension == beam_name, ]

  boxplot(
    peak_force ~ moisture_condition,
    data = beam_data,
    col = "grey92",
    border = "grey35",
    xlab = "Water added",
    ylab = "Peak force (lbf)",
    main = beam_name
  )

  for (moisture_index in seq_along(moisture_levels)) {
    moisture_prefix = moisture_levels[moisture_index]
    moisture_label = condition_prefix_to_label(moisture_prefix)
    moisture_values = beam_data$peak_force[beam_data$moisture_condition == moisture_label]

    if (length(moisture_values) == 0) next

    points(
      jitter(rep(moisture_index, length(moisture_values)), amount = 0.08),
      moisture_values,
      pch = 16,
      cex = 1,
      col = moisture_colors[moisture_prefix]
    )

    points(
      moisture_index,
      mean(moisture_values),
      pch = 18,
      cex = 1.4,
      col = "black"
    )
  }
}
