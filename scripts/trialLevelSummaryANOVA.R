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
trial_metadata$moisture_condition = factor(
  trial_metadata$moisture_condition,
  levels = condition_prefix_levels,
  labels = condition_display_labels[condition_prefix_levels]
)
trial_metadata$beam_dimension = factor(trial_metadata$beam_dimension, levels = c("Circle", "IBeam", "Square"))
trial_metadata$soil_moisture_level = condition_prefix_to_numeric(as.character(trial_metadata$condition))

fit_trial_model = function(trial_name) {
  trial_df = all_trials[[trial_name]]
  model = lm(`force[lbf]` ~ `displacement[in]`, data = trial_df)
  coeffs = coef(model)

  data.frame(
    trial = trial_name,
    intercept = unname(coeffs[1]),
    slope = unname(coeffs[2]),
    r_squared = summary(model)$r.squared,
    peak_force = max(trial_df$`force[lbf]`, na.rm = TRUE),
    peak_displacement = trial_df$`displacement[in]`[which.max(trial_df$`force[lbf]`)],
    stringsAsFactors = FALSE
  )
}

trial_coefficients = do.call(rbind, lapply(trial_names, fit_trial_model))
trial_stats = merge(trial_metadata, trial_coefficients, by = "trial", sort = FALSE)

slope_model = lm(slope ~ moisture_condition * beam_dimension, data = trial_stats)
intercept_model = lm(intercept ~ moisture_condition * beam_dimension, data = trial_stats)
peak_force_model = lm(peak_force ~ moisture_condition * beam_dimension, data = trial_stats)
peak_displacement_model = lm(peak_displacement ~ moisture_condition * beam_dimension, data = trial_stats)

slope_anova = anova(slope_model)
intercept_anova = anova(intercept_model)
peak_force_anova = anova(peak_force_model)
peak_displacement_anova = anova(peak_displacement_model)

print("Trial-level coefficient table:")
print(trial_stats)

print("ANOVA for slope:")
print(slope_anova)

print("ANOVA for intercept:")
print(intercept_anova)

print("ANOVA for peak force:")
print(peak_force_anova)

print("ANOVA for peak displacement:")
print(peak_displacement_anova)
