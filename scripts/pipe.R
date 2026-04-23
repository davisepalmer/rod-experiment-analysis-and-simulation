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

script_dir = get_script_dir()

# loadRaw.R:
# Reads the CSV files, removes excluded trials, and builds all_trials.
source(
  file.path(script_dir, "loadRaw.R"),
  chdir = TRUE
)

# trialLevelSummaryANOVA.R:
# Fits per-trial linear models and runs ANOVAs on trial-level summaries.
source(
  file.path(script_dir, "trialLevelSummaryANOVA.R"),
  chdir = TRUE
)

# pooledPointPolynomialRegression.R:
# Fits the pooled polynomial regression across all included points.
source(
  file.path(script_dir, "pooledPointPolynomialRegression.R"),
  chdir = TRUE
)

# compareDryCircleVsSlopCircle.R:
# Compares 0u-water versus 15u-water circle trials with a focused regression
# analysis.
source(
  file.path(script_dir, "compareDryCircleVsSlopCircle.R"),
  chdir = TRUE
)

# plotters_1_2.R:
# Plots one water-added/shape condition with all trials and a fitted regression curve.
source(
  file.path(script_dir, "plotters_1_2.R"),
  chdir = TRUE
)

# singleComboExample.R:
# Shows a single water-added/shape example with its fitted regression curve.
source(
  file.path(script_dir, "singleComboExample.R"),
  chdir = TRUE
)

# wetnessShapeOverlayPlots.R:
# Overlays average shape curves within each water-added condition.
source(
  file.path(script_dir, "wetnessShapeOverlayPlots.R"),
  chdir = TRUE
)

# allTrialPlots.R:
# Plots all included trials in a 3x4 layout by beam shape and water-added level.
source(
  file.path(script_dir, "allTrialPlots.R"),
  chdir = TRUE
)

# dataAnalysisMain.R:
# Fits the final pooled model, prints ANOVA results, and plots all
# condition-specific fits.
source(
  file.path(script_dir, "dataAnalysisMain.R"),
  chdir = TRUE
)

# moistureWithinShapeOverlayPlots.R:
# Overlays moisture curves within each beam shape.
source(
  file.path(script_dir, "moistureWithinShapeOverlayPlots.R"),
  chdir = TRUE
)

# peakForceSummaryPlots.R:
# Summarizes peak force by moisture condition for each beam shape.
source(
  file.path(script_dir, "peakForceSummaryPlots.R"),
  chdir = TRUE
)

# pooledVsTrialAwareComparison.R:
# Compares pooled linear modeling with a trial-aware mixed-effects approach.
source(
  file.path(script_dir, "pooledVsTrialAwareComparison.R"),
  chdir = TRUE
)
