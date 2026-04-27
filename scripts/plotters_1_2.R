# Plots all trials for one water-added/shape condition with a fitted regression curve.
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

combo_prefix = "0uWaterCircle"
poly_degree = 9
save_plots_as_png = FALSE
project_root = normalizePath(file.path(get_script_dir(), ".."))
plot_output_dir = file.path(project_root, "outputs", "plots")
if (!dir.exists(plot_output_dir)) dir.create(plot_output_dir, recursive = TRUE)
plot_output_file = file.path(plot_output_dir, paste0(combo_prefix, "_regression_plot.png"))
combo_trial_names = names(all_trials)[grepl(paste0("^", combo_prefix, "_T[0-9]+$"), names(all_trials))]
combo_trial_names = combo_trial_names[order(as.numeric(sub(".*_T", "", combo_trial_names)))]

combo_data = do.call(rbind, lapply(combo_trial_names, function(trial_name) {
  trial_df = all_trials[[trial_name]]
  data.frame(
    trial = trial_name,
    displacement_in = trial_df$`displacement[in]`,
    force_lbf = trial_df$`force[lbf]`
  )
}))

combo_data$trial = factor(combo_data$trial)

poly_terms = paste0("I(displacement_in^", 1:poly_degree, ")", collapse = " + ")
combo_regression_formula = as.formula(paste("force_lbf ~", poly_terms))
combo_trial_anova_formula = as.formula(paste("force_lbf ~ (", poly_terms, ") * trial"))

# One regression using all points from this single water-added/shape combination.
combo_regression = lm(combo_regression_formula, data = combo_data)
combo_regression_anova = anova(combo_regression)

# Regression-with-interaction to test whether trial-specific lines differ
# within this one water-added/shape combination.
combo_trial_anova_model = lm(combo_trial_anova_formula, data = combo_data)
combo_trial_anova = anova(combo_trial_anova_model)

print(paste("Regression for", combo_prefix))
print(paste("Polynomial degree =", poly_degree))
print(summary(combo_regression))

print(paste("ANOVA for regression of", combo_prefix))
print(combo_regression_anova)

print(paste("ANOVA for trial-to-trial differences within", combo_prefix))
print(combo_trial_anova)

combo_label = paste(condition_prefix_to_label(extract_condition_prefix(combo_prefix)), strip_condition_prefix(combo_prefix))

trial_colors = c("red", "blue", "darkgreen", "orange", "purple")[combo_data$trial]

if (save_plots_as_png) {
  png(filename = plot_output_file, width = 1200, height = 900, res = 150)
  on.exit(dev.off(), add = TRUE)
}

old_par = par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(family = plot_font_family)

plot(
  combo_data$displacement_in,
  combo_data$force_lbf,
  col = trial_colors,
  pch = 16,
  xlab = "Displacement (in)",
  ylab = "Force (lbf)",
  main = paste("Regression for", combo_label)
)

displacement_grid = seq(
  min(combo_data$displacement_in, na.rm = TRUE),
  max(combo_data$displacement_in, na.rm = TRUE),
  length.out = 500
)
regression_curve = predict(
  combo_regression,
  newdata = data.frame(displacement_in = displacement_grid)
)

lines(displacement_grid, regression_curve, lwd = 5)

legend(
  "topleft",
  legend = levels(combo_data$trial),
  col = c("red", "blue", "darkgreen", "orange", "purple")[seq_along(levels(combo_data$trial))],
  pch = 16,
  bty = "n"
)
