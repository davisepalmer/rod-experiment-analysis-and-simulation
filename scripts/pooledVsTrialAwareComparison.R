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
    beam_dimension = beam_dimension,
    trial_num = trial_num,
    moisture_condition = moisture_condition,
    soil_moisture_level = condition_prefix_to_numeric(prefix),
    stringsAsFactors = FALSE
  )
}

trial_metadata = do.call(rbind, lapply(trial_names, parse_trial_factors))
trial_metadata$beam_dimension = factor(trial_metadata$beam_dimension, levels = c("Circle", "IBeam", "Square"))

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
model_data$disp_c = model_data$displacement_in - mean(model_data$displacement_in, na.rm = TRUE)
model_data$moist_c = model_data$soil_moisture_level - mean(model_data$soil_moisture_level, na.rm = TRUE)

eqn = force_lbf ~
  beam_dimension +
  moist_c +
  disp_c + I(disp_c^2) + I(disp_c^3) +
  beam_dimension:moist_c +
  disp_c:beam_dimension +
  I(disp_c^2):beam_dimension +
  disp_c:moist_c +
  I(disp_c^2):moist_c

pooled_model = lm(eqn, data = model_data)
trial_aware_model = nlme::lme(
  fixed = eqn,
  random = ~ 1 | trial,
  data = model_data,
  method = "REML",
  control = nlme::lmeControl(opt = "optim")
)

print("Shared model equation:")
print(eqn)

print("Pooled model summary:")
print(summary(pooled_model))

print("Pooled model ANOVA:")
print(anova(pooled_model))

print("Trial-aware mixed model summary:")
print(summary(trial_aware_model))

print("Trial-aware mixed model ANOVA:")
print(anova(trial_aware_model))

print("Estimated random-effect variance by trial:")
print(nlme::VarCorr(trial_aware_model))

pooled_coef_summary = coef(summary(pooled_model))
trial_aware_coef_summary = summary(trial_aware_model)$tTable

all_terms = union(rownames(pooled_coef_summary), rownames(trial_aware_coef_summary))
coefficient_comparison = data.frame(
  term = all_terms,
  pooled_estimate = pooled_coef_summary[all_terms, "Estimate"],
  pooled_std_error = pooled_coef_summary[all_terms, "Std. Error"],
  pooled_t_value = pooled_coef_summary[all_terms, "t value"],
  pooled_p_value = pooled_coef_summary[all_terms, "Pr(>|t|)"],
  trial_aware_estimate = trial_aware_coef_summary[all_terms, "Value"],
  trial_aware_std_error = trial_aware_coef_summary[all_terms, "Std.Error"],
  trial_aware_t_value = trial_aware_coef_summary[all_terms, "t-value"],
  trial_aware_p_value = trial_aware_coef_summary[all_terms, "p-value"],
  estimate_difference = pooled_coef_summary[all_terms, "Estimate"] - trial_aware_coef_summary[all_terms, "Value"],
  row.names = NULL,
  check.names = FALSE
)

estimate_difference_table = coefficient_comparison[, c("term", "estimate_difference")]

print("Estimate differences between pooled and trial-aware models:")
print(estimate_difference_table)
