# Plots average force-displacement curves by shape for each water-added condition.
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

moisture_levels = condition_prefix_levels
shape_levels = c("Circle", "Ibeam", "Square")
shape_colors = c(Circle = "red", Ibeam = "blue", Square = "darkgreen")
save_plots_as_png = FALSE
project_root = normalizePath(file.path(get_script_dir(), ".."))
plot_output_dir = file.path(project_root, "outputs", "plots")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)
plot_output_file = file.path(plot_output_dir, "wetness_shape_overlay_plots.png")
axis_title_cex = 1.35
axis_tick_cex = 1.15
panel_title_cex = 1.3
legend_text_cex = 1.15

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

condition_curves = list()
for (moisture_prefix in moisture_levels) {
  condition_curves[[moisture_prefix]] = list(
    Circle = average_condition_curve(c(paste0(moisture_prefix, "Circle"))),
    Ibeam = average_condition_curve(c(paste0(moisture_prefix, "Ibeam"), paste0(moisture_prefix, "IBeam"))),
    Square = average_condition_curve(c(paste0(moisture_prefix, "Square")))
  )
}

if (save_plots_as_png) {
  png(filename = plot_output_file, width = 1600, height = 1200, res = 150)
  on.exit(dev.off(), add = TRUE)
}

par(mfrow = c(2, 2), mar = c(4.5, 4.5, 3.5, 1.5))

for (moisture_prefix in moisture_levels) {
  moisture_label = condition_prefix_to_label(moisture_prefix)
  shape_curves = condition_curves[[moisture_prefix]]
  available_shapes = names(shape_curves)[!sapply(shape_curves, is.null)]

  if (length(available_shapes) == 0) next

  x_range = range(unlist(lapply(shape_curves[available_shapes], function(df) df$displacement_in)), na.rm = TRUE)
  y_range = range(unlist(lapply(shape_curves[available_shapes], function(df) df$force_lbf)), na.rm = TRUE)

  first_shape = available_shapes[1]
  plot(
    shape_curves[[first_shape]]$displacement_in,
    shape_curves[[first_shape]]$force_lbf,
    type = "l",
    col = shape_colors[first_shape],
    lwd = 2,
    xlab = "Displacement (in)",
    ylab = "Force (lbf)",
    cex.lab = axis_title_cex,
    cex.axis = axis_tick_cex,
    cex.main = panel_title_cex,
    main = paste0(moisture_label, " conditions"),
    xlim = x_range,
    ylim = y_range
  )

  if (length(available_shapes) > 1) {
    for (shape in available_shapes[-1]) {
      lines(
        shape_curves[[shape]]$displacement_in,
        shape_curves[[shape]]$force_lbf,
        col = shape_colors[shape],
        lwd = 2
      )
    }
  }

  legend(
    "topleft",
    legend = available_shapes,
    col = shape_colors[available_shapes],
    lwd = 2,
    bty = "n"
    ,
    cex = legend_text_cex
  )
}
