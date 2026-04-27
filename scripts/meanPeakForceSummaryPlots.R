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

trial_summary = do.call(rbind, lapply(names(all_trials), function(trial_name) {
  prefix = sub("_T[0-9]+$", "", trial_name)
  moisture_prefix = extract_condition_prefix(prefix)
  beam_dimension = normalize_shape_name(strip_condition_prefix(prefix))
  trial_df = all_trials[[trial_name]]

  data.frame(
    trial = trial_name,
    treatment = prefix,
    moisture_prefix = moisture_prefix,
    moisture_condition = condition_prefix_to_label(prefix),
    beam_dimension = beam_dimension,
    peak_force = max(trial_df$`force[lbf]`, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))

trial_summary$beam_dimension = factor(trial_summary$beam_dimension, levels = c("Circle", "IBeam", "Square"))
trial_summary$moisture_prefix = factor(trial_summary$moisture_prefix, levels = condition_prefix_levels)
trial_summary$moisture_condition = factor(
  trial_summary$moisture_condition,
  levels = unname(condition_display_labels[condition_prefix_levels])
)

summarize_peak_force = function(data, group_var) {
  split_values = split(data$peak_force, data[[group_var]])

  summary_df = do.call(rbind, lapply(names(split_values), function(group_name) {
    values = split_values[[group_name]]
    n = length(values)
    mean_value = mean(values)
    sd_value = if (n > 1) sd(values) else 0
    se_value = if (n > 1) sd_value / sqrt(n) else 0
    t_value = if (n > 1) qt(0.975, df = n - 1) else 0
    ci_half_width = t_value * se_value

    data.frame(
      group = group_name,
      mean_peak_force = mean_value,
      lower_ci = mean_value - ci_half_width,
      upper_ci = mean_value + ci_half_width,
      n_trials = n,
      stringsAsFactors = FALSE
    )
  }))

  rownames(summary_df) = NULL
  summary_df
}

draw_ci_bars = function(x, lower, upper, mean_value, cap_width = 0.18) {
  segments(x0 = x, y0 = lower, x1 = x, y1 = upper, lwd = 2.5, col = "black", xpd = NA)
  segments(x0 = x - cap_width, y0 = lower, x1 = x + cap_width, y1 = lower, lwd = 2.5, col = "black", xpd = NA)
  segments(x0 = x - cap_width, y0 = upper, x1 = x + cap_width, y1 = upper, lwd = 2.5, col = "black", xpd = NA)
  points(x = x, y = mean_value, pch = 16, cex = 0.8, col = "black", xpd = NA)
}

treatment_mean = summarize_peak_force(trial_summary, "treatment")
treatment_mean = treatment_mean[order(treatment_mean$mean_peak_force, decreasing = TRUE), ]
treatment_ylim = c(0, max(treatment_mean$upper_ci, na.rm = TRUE) * 1.05)

moisture_mean = summarize_peak_force(trial_summary, "moisture_condition")
moisture_mean = moisture_mean[match(unname(condition_display_labels[condition_prefix_levels]), moisture_mean$group), ]
moisture_ylim = c(0, max(moisture_mean$upper_ci, na.rm = TRUE) * 1.05)

shape_mean = summarize_peak_force(trial_summary, "beam_dimension")
shape_mean = shape_mean[match(c("Circle", "IBeam", "Square"), shape_mean$group), ]
shape_ylim = c(0, max(shape_mean$upper_ci, na.rm = TRUE) * 1.05)

plot_mean_peak_force = function(summary_df, colors, plot_title, las = 1, cex.names = 1, ylim_override = NULL) {
  par(mfrow = c(1, 1), mar = c(8, 5, 3, 1), family = plot_font_family)
  ylim = if (is.null(ylim_override)) c(0, max(summary_df$upper_ci, na.rm = TRUE) * 1.05) else ylim_override
  bar_centers = barplot(
    height = summary_df$mean_peak_force,
    names.arg = summary_df$group,
    las = las,
    col = colors,
    border = "grey25",
    ylab = "Mean peak force (lbf)",
    main = plot_title,
    cex.names = cex.names,
    ylim = ylim
  )
  draw_ci_bars(bar_centers, summary_df$lower_ci, summary_df$upper_ci, summary_df$mean_peak_force)
}

show_plots = c("treatment", "moisture", "shape")

for (i in seq_along(show_plots)) {
  if (i > 1) dev.new()

  if (show_plots[i] == "treatment") {
    plot_mean_peak_force(
      summary_df = treatment_mean,
      colors = "grey70",
      plot_title = "Mean Peak Force by Treatment Combination",
      las = 2,
      cex.names = 0.8
    )
  }

  if (show_plots[i] == "moisture") {
    plot_mean_peak_force(
      summary_df = moisture_mean,
      colors = c("#FF3030", "#FFD700", "#228B22", "#1E90FF"),
      plot_title = "Mean Peak Force by Moisture Level"
    )
  }

  if (show_plots[i] == "shape") {
    plot_mean_peak_force(
      summary_df = shape_mean,
      colors = c("firebrick", "steelblue", "darkgreen"),
      plot_title = "Mean Peak Force by Beam Shape",
      ylim_override = c(0, 5)
    )
  }
}

format_footer_means = function(summary_df, digits = 2) {
  paste(
    paste0(summary_df$group, ": ", formatC(summary_df$mean_peak_force, digits = digits, format = "f")),
    collapse = "; "
  )
}

cat("\nCOPY/PASTE FOOTER VALUES - MEAN PEAK FORCE LEFT TO RIGHT:\n")
cat("Moisture chart: ", format_footer_means(moisture_mean, digits = 4), " [lbf].\n", sep = "")
cat("Beam shape chart: ", format_footer_means(shape_mean, digits = 4), " [lbf].\n", sep = "")

cat("\nCOPY/PASTE TABLE - MEAN PEAK FORCE:\n")
footer_table = rbind(
  data.frame(chart = "Moisture", group = moisture_mean$group, mean_peak_force_lbf = moisture_mean$mean_peak_force),
  data.frame(chart = "Beam shape", group = shape_mean$group, mean_peak_force_lbf = shape_mean$mean_peak_force)
)
footer_table$mean_peak_force_lbf = formatC(footer_table$mean_peak_force_lbf, digits = 4, format = "f")
write.table(footer_table, row.names = FALSE, quote = FALSE, sep = "\t")
