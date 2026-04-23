# Plots average force-displacement curves by moisture level for each beam shape.
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

windowsFonts(Times = windowsFont("Times New Roman"))

save_plots_as_png = FALSE
project_root = normalizePath(file.path(get_script_dir(), ".."))
plot_output_dir = file.path(project_root, "outputs", "plots")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)
plot_output_file = file.path(plot_output_dir, "moisture_within_shape_overlay_plots.png")
beam_levels = c("Circle", "IBeam", "Square")
moisture_levels = condition_prefix_levels
moisture_colors = c(
  "0uWater" = "#FF3030",
  "5uWater" = "#FFD700",
  "10uWater" = "#228B22",
  "15uWater" = "#1E90FF"
)
axis_title_cex = 1.4
axis_tick_cex = 1.2
panel_title_cex = 1.35
legend_text_cex = 1.35

average_condition_curve = function(condition_prefixes) {
  trial_names = unlist(lapply(condition_prefixes, function(prefix) {
    names(all_trials)[grepl(paste0("^", prefix, "_T[0-9]+$"), names(all_trials))]
  }))
  trial_names = unique(trial_names)
  trial_names = trial_names[order(as.numeric(sub(".*_T", "", trial_names)))]

  if (length(trial_names) == 0) return(NULL)

  trial_data = lapply(all_trials[trial_names], function(df) {
    df[order(df$`displacement[in]`), c("displacement[in]", "force[lbf]")]
  })
  common_length = min(sapply(trial_data, nrow))

  if (!is.finite(common_length) || common_length < 2) return(NULL)

  displacement_matrix = sapply(trial_data, function(df) df$`displacement[in]`[1:common_length])
  force_matrix = sapply(trial_data, function(df) df$`force[lbf]`[1:common_length])

  if (is.null(dim(displacement_matrix))) displacement_matrix = matrix(displacement_matrix, ncol = 1)
  if (is.null(dim(force_matrix))) force_matrix = matrix(force_matrix, ncol = 1)

  data.frame(
    displacement_in = rowMeans(displacement_matrix, na.rm = TRUE),
    force_lbf = rowMeans(force_matrix, na.rm = TRUE)
  )
}

shape_curves = list(
  Circle = list(
    "0uWater" = average_condition_curve("0uWaterCircle"),
    "5uWater" = average_condition_curve("5uWaterCircle"),
    "10uWater" = average_condition_curve("10uWaterCircle"),
    "15uWater" = average_condition_curve("15uWaterCircle")
  ),
  IBeam = list(
    "0uWater" = average_condition_curve(c("0uWaterIbeam", "0uWaterIBeam")),
    "5uWater" = average_condition_curve(c("5uWaterIbeam", "5uWaterIBeam")),
    "10uWater" = average_condition_curve(c("10uWaterIbeam", "10uWaterIBeam")),
    "15uWater" = average_condition_curve(c("15uWaterIbeam", "15uWaterIBeam"))
  ),
  Square = list(
    "0uWater" = average_condition_curve("0uWaterSquare"),
    "5uWater" = average_condition_curve("5uWaterSquare"),
    "10uWater" = average_condition_curve("10uWaterSquare"),
    "15uWater" = average_condition_curve("15uWaterSquare")
  )
)

if (save_plots_as_png) {
  png(filename = plot_output_file, width = 1800, height = 700, res = 150)
  on.exit(dev.off(), add = TRUE)
}

old_par = par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(
  mfrow = c(1, 3),
  mar = c(4.5, 4.5, 3.5, 1.5),
  family = "Times"
)

for (beam_name in beam_levels) {
  moisture_curves = shape_curves[[beam_name]]
  available_moisture = names(moisture_curves)[!sapply(moisture_curves, is.null)]

  if (length(available_moisture) == 0) {
    plot.new()
    title(main = beam_name)
    next
  }

  x_range = range(unlist(lapply(moisture_curves[available_moisture], function(df) df$displacement_in)), na.rm = TRUE)
  y_range = range(unlist(lapply(moisture_curves[available_moisture], function(df) df$force_lbf)), na.rm = TRUE)

  first_moisture = available_moisture[1]
  plot(
    moisture_curves[[first_moisture]]$displacement_in,
    moisture_curves[[first_moisture]]$force_lbf,
    type = "l",
    col = moisture_colors[first_moisture],
    lwd = 3,
    xlab = "Displacement (in)",
    ylab = "Force (lbf)",
    cex.lab = axis_title_cex,
    cex.axis = axis_tick_cex,
    cex.main = panel_title_cex,
    main = beam_name,
    xlim = x_range,
    ylim = y_range
  )

  if (length(available_moisture) > 1) {
    for (moisture_prefix in available_moisture[-1]) {
      lines(
        moisture_curves[[moisture_prefix]]$displacement_in,
        moisture_curves[[moisture_prefix]]$force_lbf,
        col = moisture_colors[moisture_prefix],
        lwd = 3
      )
    }
  }

  legend(
    "topleft",
    legend = condition_prefix_to_label(available_moisture),
    col = moisture_colors[available_moisture],
    lwd = 3,
    bty = "n",
    cex = legend_text_cex
  )
}
