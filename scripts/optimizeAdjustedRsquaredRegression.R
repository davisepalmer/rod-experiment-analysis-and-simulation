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

max_poly_degree = 10
min_poly_degree = 1
adjusted_r_squared_tolerance = 0.002

build_eqn = function(poly_degree) {
  displacement_terms = paste0("I(displacement_in^", seq_len(poly_degree), ")", collapse = " + ")

  as.formula(
    paste(
      "force_lbf ~ beam_dimension + soil_moisture_level +",
      displacement_terms,
      "+ beam_dimension:soil_moisture_level",
      "+ (", displacement_terms, "):beam_dimension",
      "+ (", displacement_terms, "):soil_moisture_level"
    )
  )
}

search_results = do.call(rbind, lapply(seq(max_poly_degree, min_poly_degree, by = -1), function(poly_degree) {
  eqn = build_eqn(poly_degree)
  model = lm(eqn, data = model_data)
  model_summary = summary(model)
  coefficient_table = model_summary$coefficients
  non_intercept_rows = rownames(coefficient_table) != "(Intercept)"
  significant_terms = sum(coefficient_table[non_intercept_rows, "Pr(>|t|)"] < 0.05, na.rm = TRUE)
  total_terms = sum(non_intercept_rows)

  data.frame(
    polynomial_degree = poly_degree,
    adjusted_r_squared = unname(model_summary$adj.r.squared),
    r_squared = unname(model_summary$r.squared),
    significant_terms = significant_terms,
    total_terms = total_terms,
    significant_fraction = significant_terms / total_terms,
    aic = AIC(model),
    bic = BIC(model),
    stringsAsFactors = FALSE
  )
}))

max_adjusted_r_squared = max(search_results$adjusted_r_squared)
candidate_rows = search_results[
  search_results$adjusted_r_squared >= max_adjusted_r_squared - adjusted_r_squared_tolerance,
  ,
  drop = FALSE
]
candidate_rows = candidate_rows[order(candidate_rows$polynomial_degree, -candidate_rows$significant_fraction), , drop = FALSE]
best_row = candidate_rows[1, , drop = FALSE]
best_degree = best_row$polynomial_degree[1]
best_eqn = build_eqn(best_degree)
best_model = lm(best_eqn, data = model_data)

coefficient_table = summary(best_model)$coefficients
anova_table = anova(best_model)

print("Adjusted R-squared search results:")
print(search_results)

print("Candidate models within adjusted R-squared tolerance:")
print(candidate_rows)

print("Chosen polynomial degree with parsimony rule:")
print(best_degree)

print("Best model summary:")
print(summary(best_model))

print("Best model coefficient table:")
print(coefficient_table)

print("Best model ANOVA table:")
print(anova_table)

print("Final equation:")
print(best_eqn)
