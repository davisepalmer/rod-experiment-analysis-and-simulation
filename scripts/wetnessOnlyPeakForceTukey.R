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

peak_force_table = do.call(rbind, lapply(trial_names, function(trial_name) {
  condition_prefix = extract_condition_prefix(sub("_T[0-9]+$", "", trial_name))
  trial_df = all_trials[[trial_name]]

  data.frame(
    trial = trial_name,
    moisture_condition = condition_display_labels[condition_prefix],
    soil_moisture_level = condition_prefix_to_numeric(condition_prefix),
    peak_force = max(trial_df$`force[lbf]`, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))

peak_force_table$moisture_condition = factor(
  peak_force_table$moisture_condition,
  levels = unname(condition_display_labels[condition_prefix_levels])
)

wetness_only_model = aov(peak_force ~ moisture_condition, data = peak_force_table)
wetness_only_anova = summary(wetness_only_model)
wetness_only_tukey = TukeyHSD(wetness_only_model)

wetness_summary = aggregate(
  peak_force ~ moisture_condition,
  data = peak_force_table,
  FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
)
wetness_summary = data.frame(
  moisture_condition = wetness_summary$moisture_condition,
  mean_peak_force = wetness_summary$peak_force[, "mean"],
  sd_peak_force = wetness_summary$peak_force[, "sd"],
  n_trials = wetness_summary$peak_force[, "n"]
)
wetness_summary = wetness_summary[order(wetness_summary$mean_peak_force), ]

recommended_min_wetness = wetness_summary$moisture_condition[1]

print("Peak force by trial:")
print(peak_force_table)

print("Peak force summary by wetness level:")
print(wetness_summary)

print("ANOVA for peak force across wetness levels:")
print(wetness_only_anova)

print("Tukey HSD for peak force across wetness levels:")
print(wetness_only_tukey)

print("Lowest mean peak-force wetness level:")
print(recommended_min_wetness)
