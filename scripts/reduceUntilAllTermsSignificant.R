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
    soil_moisture_level = condition_prefix_to_numeric(prefix),
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

model_data = do.call(rbind, lapply(trial_names, function(trial_name) {
  trial_df = all_trials[[trial_name]]
  meta = trial_metadata[trial_metadata$trial == trial_name, ][1, ]

  data.frame(
    trial = trial_name,
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

alpha_level = 0.05

current_eqn = force_lbf ~
  beam_dimension +
  soil_moisture_level +
  displacement_in + I(displacement_in^2) + I(displacement_in^3) +
  beam_dimension:soil_moisture_level +
  displacement_in:beam_dimension +
  I(displacement_in^2):beam_dimension +
  displacement_in:soil_moisture_level +
  I(displacement_in^2):soil_moisture_level

removed_terms = character()

repeat {
  current_model = lm(current_eqn, data = model_data)
  coefficient_table = summary(current_model)$coefficients
  non_intercept_terms = coefficient_table[rownames(coefficient_table) != "(Intercept)", , drop = FALSE]

  if (nrow(non_intercept_terms) == 0 || all(non_intercept_terms[, "Pr(>|t|)"] < alpha_level, na.rm = TRUE)) {
    break
  }

  worst_term = rownames(non_intercept_terms)[which.max(non_intercept_terms[, "Pr(>|t|)"])]
  removed_terms = c(removed_terms, worst_term)
  current_eqn = update(current_eqn, paste(". ~ . - `", worst_term, "`", sep = ""))
}

final_model = lm(current_eqn, data = model_data)
final_coefficient_table = summary(final_model)$coefficients
final_anova_table = anova(final_model)

print("Removed terms during backward elimination:")
print(removed_terms)

print("Final reduced model summary:")
print(summary(final_model))

print("Final reduced model coefficient table:")
print(final_coefficient_table)

print("Final reduced model ANOVA table:")
print(final_anova_table)

print("Final reduced equation:")
print(current_eqn)
