# Plots the final reduced all-significant regression equation against every trial.
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
plot_output_file = file.path(plot_output_dir, "reduced_equation_all_trials_overlay.png")

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

format_model_equation = function(model) {
  coefficients = coef(model)
  term_names = names(coefficients)
  term_names[term_names == "(Intercept)"] = ""

  equation_terms = sprintf("%.8g%s", abs(coefficients), ifelse(term_names == "", "", paste0("*", term_names)))
  signs = ifelse(coefficients < 0, " - ", " + ")
  equation = paste0(equation_terms[1], paste0(signs[-1], equation_terms[-1], collapse = ""))

  paste("force_lbf =", equation)
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
full_anova_table = anova(reduced_model)

cat("\nFINAL REDUCED EQUATION:\n")
cat(format_model_equation(reduced_model), "\n", sep = "")

cat("\nFULL ANOVA TABLE:\n")
print(full_anova_table)

if (save_plots_as_png) {
  png(filename = plot_output_file, width = 1800, height = 1350, res = 150)
}

shape_levels = c("Circle", "IBeam", "Square")
trial_colors = c("red", "blue", "darkgreen", "orange", "purple")
plot_font_family = "serif"

if (.Platform$OS.type == "windows") {
  windowsFonts(TimesNewRoman = windowsFont("Times New Roman"))
  plot_font_family = "TimesNewRoman"
}

if (!interactive() && !save_plots_as_png) {
  plot_font_family = "serif"
}

old_par = par(no.readonly = TRUE)
old_par$pin = NULL
on.exit(par(old_par), add = TRUE)
if (save_plots_as_png) on.exit(dev.off(), add = TRUE)

par(
  family = plot_font_family,
  mfrow = c(length(shape_levels), length(condition_prefix_levels)),
  mar = c(4, 4, 3, 1)
)

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
      lwd = 1.5,
      xlab = "Displacement (in)",
      ylab = "Force (lbf)",
      main = panel_title,
      xlim = x_range,
      ylim = y_range
    )

    if (length(panel_trial_names) > 1) {
      for (i in 2:length(panel_trial_names)) {
        lines(
          all_trials[[panel_trial_names[i]]]$`displacement[in]`,
          all_trials[[panel_trial_names[i]]]$`force[lbf]`,
          col = trial_colors[i],
          lwd = 1.5
        )
      }
    }

    displacement_grid = seq(x_range[1], x_range[2], length.out = 500)
    prediction_data = data.frame(
      beam_dimension = factor(shape_name, levels = levels(model_data$beam_dimension)),
      soil_moisture_level = condition_prefix_to_numeric(condition_prefix),
      displacement_in = displacement_grid
    )
    prediction_data = add_reduced_equation_columns(prediction_data)

    regression_curve = predict(reduced_model, newdata = prediction_data)
    lines(displacement_grid, regression_curve, col = "black", lwd = 4)
  }
}
