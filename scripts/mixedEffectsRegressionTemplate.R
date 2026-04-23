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

required_packages = c("rlang", "reformulas", "lme4")
missing_packages = required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Missing required packages: ",
      paste(missing_packages, collapse = ", "),
      ". Install them first with install.packages(c(",
      paste(sprintf('"%s"', missing_packages), collapse = ", "),
      ")) and then rerun this script."
    )
  )
}

suppressPackageStartupMessages(library(lme4))

if (!exists("%||%", mode = "function")) {
  `%||%` = function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
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
model_data$disp_c = model_data$displacement_in - mean(model_data$displacement_in, na.rm = TRUE)
model_data$moist_c = model_data$soil_moisture_level - mean(model_data$soil_moisture_level, na.rm = TRUE)

###########################
###########################
###########################
###########################

# Recommended plain-lm fallback if you need a single non-mixed model.
lm_eqn_recommended = force_lbf ~
  beam_dimension +
  moist_c +
  disp_c + I(disp_c^2) + I(disp_c^3) +
  beam_dimension:moist_c +
  disp_c:beam_dimension +
  I(disp_c^2):beam_dimension +
  disp_c:moist_c +
  I(disp_c^2):moist_c

# Edit this line freely.
eqn = force_lbf ~
  beam_dimension +
  moist_c +
  disp_c + I(disp_c^2) + I(disp_c^3) +
  beam_dimension:moist_c +
  disp_c:beam_dimension +
  I(disp_c^2):beam_dimension +
  disp_c:moist_c +
  I(disp_c^2):moist_c +
  (1 + disp_c | trial)

# Simpler alternative if the random-slope fit is singular:
# eqn = force_lbf ~
#   beam_dimension +
#   moist_c +
#   disp_c + I(disp_c^2) + I(disp_c^3) +
#   beam_dimension:moist_c +
#   disp_c:beam_dimension +
#   I(disp_c^2):beam_dimension +
#   disp_c:moist_c +
#   I(disp_c^2):moist_c +
#   (1 | trial)

model = lmer(eqn, data = model_data, REML = FALSE)

show_diagnostics = TRUE

model_fitted = fitted(model)
model_residuals = resid(model)

if (show_diagnostics) {
  old_par = par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  
  plot(
    model_fitted,
    model_residuals,
    xlab = "Fitted values",
    ylab = "Residuals",
    main = "Residuals vs Fitted"
  )
  abline(h = 0, lty = 2, col = "gray")
  
  qqnorm(model_residuals, main = "Q-Q Residuals")
  qqline(model_residuals, col = "red")
  
  plot(
    model_fitted,
    sqrt(abs(model_residuals)),
    xlab = "Fitted values",
    ylab = expression(sqrt("|Residuals|")),
    main = "Scale-Location"
  )
  
  boxplot(
    model_residuals ~ model_data$trial,
    las = 2,
    cex.axis = 0.6,
    xlab = "Trial",
    ylab = "Residuals",
    main = "Residuals by Trial"
  )
}

###########################
###########################
###########################
###########################

model_summary = summary(model)
fixed_effects_table = model_summary$coefficients
random_effects = as.data.frame(VarCorr(model))
anova_table = drop1(model, test = "Chisq")
aic_value = AIC(model)
bic_value = BIC(model)
loglik_value = logLik(model)
fixed_effect_names = names(fixef(model))
fixed_effect_matrix_all = model.matrix(delete.response(terms(lm_eqn_recommended)), data = model_data)
fixed_only_fitted = as.vector(fixed_effect_matrix_all[, fixed_effect_names, drop = FALSE] %*% fixef(model))
var_fixed = var(fixed_only_fitted)
var_random = sum(random_effects$vcov[random_effects$grp != "Residual"])
var_residual = random_effects$vcov[random_effects$grp == "Residual"][1]
marginal_r_squared = var_fixed / (var_fixed + var_random + var_residual)
conditional_r_squared = (var_fixed + var_random) / (var_fixed + var_random + var_residual)

print("Mixed-effects regression formula:")
print(eqn)

print("Recommended plain lm formula:")
print(lm_eqn_recommended)

print("Model fit statistics:")
print(c(AIC = aic_value, BIC = bic_value, logLik = loglik_value))

print("Fixed-effects coefficient table:")
print(fixed_effects_table)

print("Random-effects structure:")
print(random_effects)

print("Likelihood-ratio ANOVA table:")
print(anova_table)

shape_levels = c("Circle", "IBeam", "Square")
trial_colors = c("red", "blue", "darkgreen", "orange", "purple")

old_par_overlay = par(no.readonly = TRUE)
on.exit(par(old_par_overlay), add = TRUE)
par(mfrow = c(length(shape_levels), length(condition_prefix_levels)), mar = c(4, 4, 3, 1))

for (shape_name in shape_levels) {
  for (condition_prefix in condition_prefix_levels) {
    panel_prefixes = c(
      paste0(condition_prefix, shape_name),
      paste0(condition_prefix, sub("IBeam", "Ibeam", shape_name))
    )
    panel_trial_names = unique(unlist(lapply(panel_prefixes, function(prefix) {
      names(all_trials)[grepl(paste0("^", prefix, "_T[0-9]+$"), names(all_trials))]
    })))
    panel_trial_names = panel_trial_names[order(as.numeric(sub(".*_T", "", panel_trial_names)))]
    
    panel_title = paste(condition_prefix_to_label(condition_prefix), shape_name)
    
    if (length(panel_trial_names) == 0) {
      plot.new()
      title(main = panel_title)
      next
    }
    
    x_range = range(unlist(lapply(panel_trial_names, function(name) all_trials[[name]]$`displacement[in]`)), na.rm = TRUE)
    y_range = range(unlist(lapply(panel_trial_names, function(name) all_trials[[name]]$`force[lbf]`)), na.rm = TRUE)
    
    plot(
      all_trials[[panel_trial_names[1]]]$`displacement[in]`,
      all_trials[[panel_trial_names[1]]]$`force[lbf]`,
      type = "l",
      col = trial_colors[1],
      lwd = 2,
      xlab = "Displacement (in)",
      ylab = "Force (lbf)",
      main = panel_title,
      xlim = x_range,
      ylim = y_range
    )
    
    if (length(panel_trial_names) > 1) {
      for (i in 2:length(panel_trial_names)) {
        lines(
          all_trials[[panel_trial_names[i]]]$`displacement[in]`,
          all_trials[[panel_trial_names[i]]]$`force[lbf]`,
          col = trial_colors[i],
          lwd = 2
        )
      }
    }
    
    displacement_grid = seq(x_range[1], x_range[2], length.out = 300)
    prediction_data = data.frame(
      trial = factor(levels(model_data$trial)[1], levels = levels(model_data$trial)),
      beam_dimension = factor(shape_name, levels = levels(model_data$beam_dimension)),
      soil_moisture_level = condition_prefix_to_numeric(condition_prefix),
      displacement_in = displacement_grid
    )
    prediction_data$disp_c = prediction_data$displacement_in - mean(model_data$displacement_in, na.rm = TRUE)
    prediction_data$moist_c = prediction_data$soil_moisture_level - mean(model_data$soil_moisture_level, na.rm = TRUE)
    
    fixed_effect_matrix = model.matrix(delete.response(terms(lm_eqn_recommended)), data = prediction_data)
    fixed_effect_matrix = fixed_effect_matrix[, fixed_effect_names, drop = FALSE]
    regression_curve = as.vector(fixed_effect_matrix %*% fixef(model))
    lines(displacement_grid, regression_curve, col = "black", lwd = 4)
  }
}

print("Mixed-model R-squared:")
print(c(Marginal = marginal_r_squared, Conditional = conditional_r_squared))
