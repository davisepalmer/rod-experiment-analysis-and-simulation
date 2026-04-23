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

peak_force_table = do.call(rbind, lapply(trial_names, function(trial_name) {
  prefix = sub("_T[0-9]+$", "", trial_name)
  trial_df = all_trials[[trial_name]]

  data.frame(
    trial = trial_name,
    moisture_condition = condition_prefix_to_label(prefix),
    beam_dimension = normalize_shape_name(strip_condition_prefix(prefix)),
    peak_force = max(trial_df$`force[lbf]`, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}))

peak_force_table$beam_dimension = factor(
  peak_force_table$beam_dimension,
  levels = c("Circle", "IBeam", "Square")
)

beam_shape_model = aov(peak_force ~ beam_dimension, data = peak_force_table)
beam_shape_anova = summary(beam_shape_model)
beam_shape_tukey = TukeyHSD(beam_shape_model)

beam_shape_summary = aggregate(
  peak_force ~ beam_dimension,
  data = peak_force_table,
  FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x))
)
beam_shape_summary = data.frame(
  beam_dimension = beam_shape_summary$beam_dimension,
  mean_peak_force = beam_shape_summary$peak_force[, "mean"],
  sd_peak_force = beam_shape_summary$peak_force[, "sd"],
  n_trials = beam_shape_summary$peak_force[, "n"]
)
beam_shape_summary = beam_shape_summary[order(beam_shape_summary$mean_peak_force), ]

recommended_min_shape = beam_shape_summary$beam_dimension[1]

print("Peak force by trial:")
print(peak_force_table)

print("Peak force summary by beam shape:")
print(beam_shape_summary)

print("ANOVA for peak force across beam shapes:")
print(beam_shape_anova)

print("Tukey HSD for peak force across beam shapes:")
print(beam_shape_tukey)

print("Lowest mean peak-force beam shape:")
print(recommended_min_shape)
