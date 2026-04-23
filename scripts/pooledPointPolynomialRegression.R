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

normalize_shape_name = function(shape_name) {
  shape_lookup = c(
    circle = "Circle",
    ibeam = "IBeam",
    square = "Square"
  )

  shape_lookup[[tolower(shape_name)]]
}

trial_names = names(all_trials)

parse_trial_factors = function(trial_name) {
  prefix = sub("_T[0-9]+$", "", trial_name)
  moisture_condition = extract_condition_prefix(prefix)
  beam_dimension = normalize_shape_name(strip_condition_prefix(prefix))

  data.frame(
    trial = trial_name,
    condition = prefix,
    moisture_condition = moisture_condition,
    beam_dimension = beam_dimension,
    stringsAsFactors = FALSE
  )
}

trial_metadata = do.call(rbind, lapply(trial_names, parse_trial_factors))
trial_metadata$moisture_condition = factor(
  trial_metadata$moisture_condition,
  levels = condition_prefix_levels,
  labels = condition_display_labels[condition_prefix_levels]
)
trial_metadata$beam_dimension = factor(trial_metadata$beam_dimension, levels = c("Circle", "IBeam", "Square"))
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

poly_degree = 10
moisture_poly_degree = 1
displacement_terms = paste0("I(displacement_in^", 1:poly_degree, ")", collapse = " + ")
moisture_terms = paste0("I(soil_moisture_level^", 1:moisture_poly_degree, ")", collapse = " + ")
pooled_regression_formula = as.formula(
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

pooled_regression_model = lm(pooled_regression_formula, data = model_data)
pooled_regression_coefficients = summary(pooled_regression_model)$coefficients
pooled_regression_anova = anova(pooled_regression_model)

print("Pooled point-level regression formula:")
print(pooled_regression_formula)

print("Summary for pooled point-level regression:")
print(summary(pooled_regression_model))

print("Coefficient table for pooled point-level regression:")
print(pooled_regression_coefficients)

print("ANOVA for pooled point-level regression:")
print(pooled_regression_anova)
