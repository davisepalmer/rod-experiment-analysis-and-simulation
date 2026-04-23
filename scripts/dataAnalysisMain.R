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

trial_names = names(all_trials)

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
  trial_num = as.integer(sub(".*_T", "", trial_name))
  moisture_condition = extract_condition_prefix(prefix)
  beam_dimension = normalize_shape_name(strip_condition_prefix(prefix))

  data.frame(
    trial = trial_name,
    condition = prefix,
    moisture_condition = moisture_condition,
    beam_dimension = beam_dimension,
    trial_num = trial_num,
    stringsAsFactors = FALSE
  )
}

trial_metadata = do.call(rbind, lapply(trial_names, parse_trial_factors))
trial_metadata$beam_dimension = factor(trial_metadata$beam_dimension, levels = c("Circle", "IBeam", "Square"))
trial_metadata$moisture_condition = factor(
  trial_metadata$moisture_condition,
  levels = condition_prefix_levels,
  labels = condition_display_labels[condition_prefix_levels]
)
trial_metadata$soil_moisture_level = condition_prefix_to_numeric(as.character(trial_metadata$condition))

model_data = do.call(rbind, lapply(trial_names, function(trial_name) {
  trial_df = all_trials[[trial_name]]
  meta = trial_metadata[trial_metadata$trial == trial_name, ][1, ]

  data.frame(
    trial = trial_name,
    beam_dimension = meta$beam_dimension,
    soil_moisture_level = meta$soil_moisture_level,
    displacement_in = trial_df$`displacement[in]`,
    force_lbf = trial_df$`force[lbf]`,
    stringsAsFactors = FALSE
  )
}))

model_data$trial = factor(model_data$trial)
model_data$beam_dimension = factor(model_data$beam_dimension, levels = levels(trial_metadata$beam_dimension))


#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
if (!exists("eqn", inherits = TRUE)) {
  poly_degree = 3
  moisture_poly_degree = 2
  displacement_terms = paste0("I(displacement_in^", 1:poly_degree, ")", collapse = " + ")
  moisture_terms = paste0("I(soil_moisture_level^", 1:moisture_poly_degree, ")", collapse = " + ")
  eqn = as.formula(
    paste(
      "force_lbf ~ beam_dimension +",
      moisture_terms,
      "+",
      displacement_terms,
      "+ beam_dimension:(", moisture_terms, ")",
      "+ (", displacement_terms, "):beam_dimension",
      "+ (", displacement_terms, "):(", moisture_terms, ")"
    )
  )
}

print("Regression formula:")
print(eqn)
    
model = lm(eqn, data = model_data)

summary(model)
#plot(model)
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
#######################################################
data_analysis_anova = anova(model)

print("Regression coefficients:")
print(coef(model))

print("ANOVA for regression model:")
print(data_analysis_anova)

moisture_levels = condition_numeric_levels
beam_levels = levels(model_data$beam_dimension)
trial_palette = c("red", "blue", "darkgreen", "orange", "purple")

old_par = par(no.readonly = TRUE)
par(mfrow = c(length(moisture_levels), length(beam_levels)), mar = c(4, 4, 3, 1))

for (moisture_prefix in names(moisture_levels)) {
  moisture_label = condition_prefix_to_label(moisture_prefix)

  for (beam_name in beam_levels) {
    plot_subset = model_data[
      model_data$beam_dimension == beam_name &
      model_data$soil_moisture_level == moisture_levels[[moisture_prefix]],
    ]

    if (nrow(plot_subset) == 0) {
      plot.new()
      title(main = paste(moisture_label, beam_name))
      next
    }

    plot_subset$trial = factor(plot_subset$trial)
    trial_colors = trial_palette[seq_along(levels(plot_subset$trial))]
    point_colors = trial_colors[plot_subset$trial]

    plot(
      plot_subset$displacement_in,
      plot_subset$force_lbf,
      pch = 16,
      cex = 0.5,
      col = point_colors,
      xlab = "Displacement (in)",
      ylab = "Force (lbf)",
      main = paste(moisture_label, beam_name)
    )

    plot_grid = data.frame(
      displacement_in = seq(min(plot_subset$displacement_in), max(plot_subset$displacement_in), length.out = 300),
      beam_dimension = factor(beam_name, levels = levels(model_data$beam_dimension)),
      soil_moisture_level = moisture_levels[[moisture_prefix]]
    )

    plot_predictions = predict(model, newdata = plot_grid)

    lines(plot_grid$displacement_in, plot_predictions, col = "black", lwd = 3)
  }
}

par(old_par)
