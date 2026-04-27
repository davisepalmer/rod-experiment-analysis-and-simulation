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
alpha_level = 0.05

add_regression_columns = function(data, poly_degree) {
  data$IB = as.integer(data$beam_dimension == "IBeam")
  data$SQ = as.integer(data$beam_dimension == "Square")
  data$M = data$soil_moisture_level
  data$D = data$displacement_in

  if (poly_degree >= 2) {
    for (degree in 2:poly_degree) {
      data[[paste0("D", degree)]] = data$D^degree
    }
  }

  data$IBxM = data$IB * data$M
  data$SQxM = data$SQ * data$M

  for (degree in seq_len(poly_degree)) {
    displacement_term = if (degree == 1) "D" else paste0("D", degree)
    data[[paste0("IBx", displacement_term)]] = data$IB * data[[displacement_term]]
    data[[paste0("SQx", displacement_term)]] = data$SQ * data[[displacement_term]]
    data[[paste0("Mx", displacement_term)]] = data$M * data[[displacement_term]]
  }

  data
}

build_terms = function(poly_degree) {
  displacement_terms = c("D", if (poly_degree >= 2) paste0("D", 2:poly_degree) else character())

  c(
    "IB", "SQ", "M",
    displacement_terms,
    "IBxM", "SQxM",
    paste0("IBx", displacement_terms),
    paste0("SQx", displacement_terms),
    paste0("Mx", displacement_terms)
  )
}

build_eqn = function(term_names) {
  reformulate(term_names, response = "force_lbf")
}

reduce_until_all_terms_significant = function(term_names, data, alpha = 0.05) {
  removed_terms = character()

  repeat {
    eqn = build_eqn(term_names)
    model = lm(eqn, data = data)
    coefficient_table = summary(model)$coefficients
    non_intercept_terms = coefficient_table[rownames(coefficient_table) != "(Intercept)", , drop = FALSE]
    p_values = non_intercept_terms[, "Pr(>|t|)"]
    p_values[is.na(p_values)] = Inf

    if (nrow(non_intercept_terms) == 0 || all(p_values < alpha)) {
      return(list(model = model, eqn = eqn, terms = term_names, removed_terms = removed_terms))
    }

    worst_term = rownames(non_intercept_terms)[which.max(p_values)]
    removed_terms = c(removed_terms, worst_term)
    term_names = setdiff(term_names, worst_term)
  }
}

format_model_equation = function(model) {
  coefficients = coef(model)
  term_names = names(coefficients)
  term_names[term_names == "(Intercept)"] = "Intercept"

  paste(
    "force_lbf =",
    paste(sprintf("%+.8g*%s", coefficients, term_names), collapse = " "),
    collapse = " "
  )
}

model_data = add_regression_columns(model_data, max_poly_degree)

search_results = do.call(rbind, lapply(seq(max_poly_degree, min_poly_degree, by = -1), function(poly_degree) {
  eqn = build_eqn(build_terms(poly_degree))
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
best_terms = build_terms(best_degree)
best_eqn = build_eqn(best_terms)
best_model = lm(best_eqn, data = model_data)
reduced_result = reduce_until_all_terms_significant(best_terms, model_data, alpha_level)
final_eqn = reduced_result$eqn
final_model = reduced_result$model

coefficient_table = summary(best_model)$coefficients
anova_table = anova(best_model)
final_coefficient_table = summary(final_model)$coefficients
final_anova_table = anova(final_model)

print("Adjusted R-squared search results:")
print(search_results)

print("Candidate models within adjusted R-squared tolerance:")
print(candidate_rows)

print("Chosen polynomial degree with parsimony rule:")
print(best_degree)

print("Chosen adjusted R-squared model summary before significance reduction:")
print(summary(best_model))

print("Chosen adjusted R-squared coefficient table before significance reduction:")
print(coefficient_table)

print("Chosen adjusted R-squared ANOVA table before significance reduction:")
print(anova_table)

print("Chosen adjusted R-squared equation before significance reduction:")
print(best_eqn)

print("Removed terms from chosen model during significance reduction:")
print(reduced_result$removed_terms)

print("Final all-significant model summary:")
print(summary(final_model))

print("Final all-significant coefficient table:")
print(final_coefficient_table)

print("Final all-significant ANOVA table:")
print(final_anova_table)

print("Final all-significant equation:")
print(final_eqn)

print("Final all-significant equation with coefficients:")
print(format_model_equation(final_model))
