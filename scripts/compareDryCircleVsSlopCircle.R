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

target_prefixes = c("0uWaterCircle", "15uWaterCircle")
poly_degree = 2

trial_names = names(all_trials)[sub("_T[0-9]+$", "", names(all_trials)) %in% target_prefixes]

fit_trial_model = function(trial_name, poly_degree) {
  trial_df = all_trials[[trial_name]]
  poly_terms = paste0("I(`displacement[in]`^", 1:poly_degree, ")", collapse = " + ")
  model_formula = as.formula(paste("`force[lbf]` ~", poly_terms))
  model = lm(model_formula, data = trial_df)
  coeffs = coef(model)

  prefix = sub("_T[0-9]+$", "", trial_name)

  data.frame(
    trial = trial_name,
    condition = prefix,
    moisture_condition = extract_condition_prefix(prefix),
    shape = "Circle",
    intercept = unname(coeffs[1]),
    linear_term = ifelse(length(coeffs) >= 2, unname(coeffs[2]), NA_real_),
    quadratic_term = ifelse(length(coeffs) >= 3, unname(coeffs[3]), NA_real_),
    peak_force = max(trial_df$`force[lbf]`, na.rm = TRUE),
    peak_displacement = trial_df$`displacement[in]`[which.max(trial_df$`force[lbf]`)],
    r_squared = summary(model)$r.squared,
    stringsAsFactors = FALSE
  )
}

comparison_stats = do.call(rbind, lapply(trial_names, fit_trial_model, poly_degree = poly_degree))
comparison_stats$condition = factor(comparison_stats$condition, levels = target_prefixes)
comparison_stats$moisture_condition = factor(
  comparison_stats$moisture_condition,
  levels = c("0uWater", "15uWater"),
  labels = condition_display_labels[c("0uWater", "15uWater")]
)

intercept_model = lm(intercept ~ condition, data = comparison_stats)
linear_model = lm(linear_term ~ condition, data = comparison_stats)
quadratic_model = lm(quadratic_term ~ condition, data = comparison_stats)
peak_force_model = lm(peak_force ~ condition, data = comparison_stats)
peak_displacement_model = lm(peak_displacement ~ condition, data = comparison_stats)

intercept_anova = anova(intercept_model)
linear_anova = anova(linear_model)
quadratic_anova = anova(quadratic_model)
peak_force_anova = anova(peak_force_model)
peak_displacement_anova = anova(peak_displacement_model)

print("Trial-level comparison table for 0uWaterCircle vs 15uWaterCircle:")
print(comparison_stats)

print("ANOVA for intercept:")
print(intercept_anova)

print("ANOVA for linear term:")
print(linear_anova)

print("ANOVA for quadratic term:")
print(quadratic_anova)

print("ANOVA for peak force:")
print(peak_force_anova)

print("ANOVA for peak displacement:")
print(peak_displacement_anova)
