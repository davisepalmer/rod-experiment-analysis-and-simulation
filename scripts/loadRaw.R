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

project_root = normalizePath(file.path(get_script_dir(), ".."))
raw_data_dir = file.path(project_root, "raw_data")
files = list.files(path = raw_data_dir, pattern = "*.csv$", ignore.case = TRUE, full.names = TRUE)

condition_prefix_levels = c("0uWater", "5uWater", "10uWater", "15uWater")
condition_display_labels = c(
  "0uWater" = "0u water",
  "5uWater" = "5u water",
  "10uWater" = "10u water",
  "15uWater" = "15u water"
)
condition_numeric_levels = c(
  "0uWater" = 0,
  "5uWater" = 5,
  "10uWater" = 10,
  "15uWater" = 15
)
condition_prefix_pattern = paste0("^(", paste(condition_prefix_levels, collapse = "|"), ")")

normalize_condition_prefix = function(x) {
  x
}

extract_condition_prefix = function(x) {
  normalized = normalize_condition_prefix(x)
  matches = regmatches(normalized, regexpr(condition_prefix_pattern, normalized))
  matches[matches == ""] = NA_character_
  matches
}

strip_condition_prefix = function(x) {
  sub(condition_prefix_pattern, "", normalize_condition_prefix(x))
}

condition_prefix_to_label = function(x) {
  unname(condition_display_labels[extract_condition_prefix(x)])
}

condition_prefix_to_numeric = function(x) {
  unname(condition_numeric_levels[extract_condition_prefix(x)])
}

excluded_trials = c("0uWaterCircle_T3", "0uWaterIBeam_T2", "5uWaterSquare_T1")

rm(
  list = c(excluded_trials)[c(excluded_trials) %in% ls(envir = .GlobalEnv)],
  envir = .GlobalEnv
)

all_trials = list()
for (file in files) {
  raw = read.csv(file, header = FALSE, stringsAsFactors = FALSE, fill = TRUE, colClasses = "character", check.names = FALSE)
  prefix = normalize_condition_prefix(sub("1$", "", tools::file_path_sans_ext(basename(file))))
  
  for (i in 1:nrow(raw)) {
    row = trimws(as.character(raw[i, 1:4]))
    row[row == ""] = NA
    
    if (all(is.na(row))) next
    if (any(grepl("[A-Za-z]", row[!is.na(row)]))) next
    
    numeric_row = suppressWarnings(as.numeric(row))
    
    if (any(is.na(numeric_row))) next
    
    trial_num = numeric_row[1]
    
    if (!is.na(trial_num) && trial_num == floor(trial_num) && trial_num >= 1) {
      name = paste0(prefix, "_T", trial_num)
      if (name %in% excluded_trials) next
      all_trials[[name]] = rbind(all_trials[[name]], numeric_row)
    }
  }
}

all_trials = all_trials[!(names(all_trials) %in% excluded_trials)]

for (k in names(all_trials)) {
  all_trials[[k]] = as.data.frame(all_trials[[k]])
  colnames(all_trials[[k]]) = c("trialNum","time[s]","displacement[in]","force[lbf]")
  end_time = max(all_trials[[k]]$`time[s]`, na.rm = TRUE)
  all_trials[[k]] = all_trials[[k]][all_trials[[k]]$`time[s]` <= end_time - 5, ]
  rownames(all_trials[[k]]) = NULL
  assign(k, all_trials[[k]], envir = .GlobalEnv)
}


