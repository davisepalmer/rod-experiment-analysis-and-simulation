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

add_regression_columns = function(data) {
  data$IB = as.integer(data$beam_dimension == "IBeam")
  data$SQ = as.integer(data$beam_dimension == "Square")
  data$M = data$soil_moisture_level
  data$D = data$displacement_in
  data$D2 = data$D^2
  data$D3 = data$D^3

  data$IBxM = data$IB * data$M
  data$SQxM = data$SQ * data$M
  data$IBxD = data$IB * data$D
  data$SQxD = data$SQ * data$D
  data$IBxD2 = data$IB * data$D2
  data$SQxD2 = data$SQ * data$D2
  data$MxD = data$M * data$D
  data$MxD2 = data$M * data$D2

  data
}

build_eqn = function(term_names) {
  reformulate(term_names, response = "force_lbf")
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

format_readable_equation = function(model) {
  coefficients = coef(model)
  term_names = names(coefficients)
  term_names[term_names == "(Intercept)"] = ""

  equation_terms = sprintf("%.8g%s", abs(coefficients), ifelse(term_names == "", "", paste0("*", term_names)))
  signs = ifelse(coefficients < 0, " - ", " + ")
  equation = paste0(equation_terms[1], paste0(signs[-1], equation_terms[-1], collapse = ""))

  paste("force_lbf =", equation)
}

format_wide_copy_paste_table = function(coefficient_table) {
  term_names = rownames(coefficient_table)
  term_names[term_names == "(Intercept)"] = "Int"

  value_matrix = rbind(
    formatC(coefficient_table[, "Estimate"], digits = 8, format = "fg", flag = "#"),
    format.pval(coefficient_table[, "Pr(>|t|)"], digits = 4, eps = 0.0001)
  )

  table_data = data.frame(
    statistic = c("Coefficient", "p-value"),
    value_matrix,
    row.names = NULL,
    check.names = FALSE
  )

  names(table_data) = c("", term_names)
  table_data
}

model_data = add_regression_columns(model_data)

current_terms = c(
  "IB", "SQ", "M", "D", "D2", "D3",
  "IBxM", "SQxM", "IBxD", "SQxD", "IBxD2", "SQxD2", "MxD", "MxD2"
)

removed_terms = character()

repeat {
  current_eqn = build_eqn(current_terms)
  current_model = lm(current_eqn, data = model_data)
  coefficient_table = summary(current_model)$coefficients
  non_intercept_terms = coefficient_table[rownames(coefficient_table) != "(Intercept)", , drop = FALSE]
  p_values = non_intercept_terms[, "Pr(>|t|)"]
  p_values[is.na(p_values)] = Inf

  if (nrow(non_intercept_terms) == 0 || all(p_values < alpha_level)) {
    break
  }

  worst_term = rownames(non_intercept_terms)[which.max(p_values)]
  removed_terms = c(removed_terms, worst_term)
  current_terms = setdiff(current_terms, worst_term)
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

print("Final reduced equation with coefficients:")
print(format_model_equation(final_model))

cat("\nCOPY/PASTE FINAL EQUATION:\n")
cat(format_readable_equation(final_model), "\n", sep = "")

cat("\nCOPY/PASTE COEFFICIENTS AND P-VALUES:\n")
write.table(
  format_wide_copy_paste_table(final_coefficient_table),
  row.names = FALSE,
  quote = FALSE,
  sep = "\t"
)
