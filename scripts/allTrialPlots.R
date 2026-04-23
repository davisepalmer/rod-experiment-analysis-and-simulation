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

normalize_shape_name = function(shape_name) {
  shape_lookup = c(
    circle = "Circle",
    ibeam = "IBeam",
    square = "Square"
  )

  shape_lookup[[tolower(shape_name)]]
}

shape_levels = c("Circle", "IBeam", "Square")
trial_colors = c("red", "blue", "darkgreen", "orange", "purple")

old_par = par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(
  mfrow = c(length(shape_levels), length(condition_prefix_levels)),
  mar = c(4, 4, 3, 1),
  family = "Times"
)
axis_title_cex = 1.4
legend_text_cex = 1.3
axis_tick_cex = 1.2
panel_title_cex = 1.3

for (shape_name in shape_levels) {
  for (condition_prefix in condition_prefix_levels) {
    panel_prefixes = c(
      paste0(condition_prefix, shape_name),
      paste0(condition_prefix, sub("IBeam", "Ibeam", shape_name))
    )
    panel_trial_names = unique(unlist(lapply(panel_prefixes, function(prefix) {
      names(all_trials)[grepl(paste0("^", prefix, "_T[0-9]+$"), names(all_trials))]
    })))
    panel_trial_names = panel_trial_names[order(as.numeric(sub(".*_T", "", panel_trial_names)))]

    panel_title = paste(condition_prefix_to_label(condition_prefix), shape_name)

    if (length(panel_trial_names) == 0) {
      plot.new()
      title(main = panel_title)
      next
    }

    x_range = range(unlist(lapply(panel_trial_names, function(name) all_trials[[name]]$`displacement[in]`)), na.rm = TRUE)
    y_range = range(unlist(lapply(panel_trial_names, function(name) all_trials[[name]]$`force[lbf]`)), na.rm = TRUE)

    plot(
      all_trials[[panel_trial_names[1]]]$`displacement[in]`,
      all_trials[[panel_trial_names[1]]]$`force[lbf]`,
      type = "l",
      col = trial_colors[1],
      lwd = 2,
      xlab = "Displacement (in)",
      ylab = "Force (lbf)",
      cex.lab = axis_title_cex,
      cex.axis = axis_tick_cex,
      cex.main = panel_title_cex,
      main = panel_title,
      xlim = x_range,
      ylim = y_range
    )

    legend_names = sub(".*_T", "T", panel_trial_names[1])
    legend_cols = trial_colors[1]

    if (length(panel_trial_names) > 1) {
      for (i in 2:length(panel_trial_names)) {
        lines(
          all_trials[[panel_trial_names[i]]]$`displacement[in]`,
          all_trials[[panel_trial_names[i]]]$`force[lbf]`,
          col = trial_colors[i],
          lwd = 2
        )

        legend_names = c(legend_names, sub(".*_T", "T", panel_trial_names[i]))
        legend_cols = c(legend_cols, trial_colors[i])
      }
    }

    legend(
      "topleft",
      legend = legend_names,
      col = legend_cols,
      lwd = 2,
      bty = "n",
      cex = legend_text_cex
    )
  }
}
