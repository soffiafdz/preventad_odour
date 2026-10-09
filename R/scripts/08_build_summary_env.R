#!/usr/bin/env Rscript
# =============================================================================
# 08_build_summary_env.R - Gather every number the summary documents use
# =============================================================================
# Reads the result files of 01 to 07 and 10 to 17 and saves, as one named
# list, every number and table that reports-src/summary.qmd and slides.qmd
# show. The documents do no analysis of their own. Time scales whose model
# selection is pending are listed as such.
#
# INPUTS:
#   - data/derivatives/smell.rds, pet.rds: from 01 and 04
#   - models/results/: results of 02, 03, 05, 06, 07, 10 to 17
# OUTPUTS:
#   - outputs/summary_env.rds: numbers, tables and figure paths
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(nlme)
  library(survival)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))

log_script_start("08_build_summary_env.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "build_summary_env", default = FALSE
)
output.path <- get_data_path("outputs", "summary_env")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("08_build_summary_env.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
PET <- get_parameter("pet")
PATH <- get_parameter("pathology")
TRAJ <- get_parameter("trajectories")
selected.v <- unlist(TRAJ$selected_model)
SCALES <- TRAJ$time_scales
ready.v <- SCALES[selected.v[SCALES] != "pending"]
pending.v <- setdiff(SCALES, ready.v)

log_section("Part 1: Data")
smell.res <- read_rds_safe(get_data_path("derivatives", "smell"))
data.lst <- list(
  extract_participants = smell.res$n$participants,
  extract_assessments = smell.res$n$assessments,
  corrected_dates = smell.res$n$corrected_dates,
  excluded_visit_rows = smell.res$data[
    get(TRAJ$exclude_col) %in% TRAJ$exclude_values, .N
  ],
  excluded_visits = TRAJ$exclude_values,
  min_assessments = TRAJ$min_assessments,
  score_range = SMELL$score_range
)

log_section("Part 2: Trajectory classes")
#' Numbers for one time scale's selected trajectory model
#' @param scale Time scale
#' @return Named list
trajectory_numbers.fn <- function(scale) {
  description.res <- read_rds_safe(
    get_data_path("models", "trajectory_description", scale)
  )
  comparison.dt <- description.res$comparison
  selected.dt <- comparison.dt[selected == TRUE]
  stopifnot(nrow(selected.dt) == 1)
  summary.dt <- description.res$class_summary
  classes.dt <- summary.dt[group != "Total"]
  total.dt <- summary.dt[group == "Total"]
  # Predicted class means at round ages (or years) inside each class's range
  points.v <- if (scale == "age") c(60, 70, 80) else c(0, 5, 10)
  predicted.dt <- description.res$curves[
    time %in% points.v, .(class, time, fit = round(fit, 1))
  ]
  list(
    n_models = nrow(comparison.dt),
    n_starts = TRAJ$gridsearch_rep[[scale]],
    degree = as.character(selected.dt$degree),
    random = as.character(selected.dt$random),
    n_classes = selected.dt$n_classes,
    bic = selected.dt$bic,
    comparison = comparison.dt,
    participants = total.dt$n,
    assessments = description.res$n$assessments,
    classes = classes.dt,
    total = total.dt,
    posterior_range = range(classes.dt$posterior_mean),
    predicted = predicted.dt,
    class_table = description.res$class_table,
    figure = paste0(
      get_data_path("outputs", "trajectory_figure", scale), ".png"
    ),
    slide_figure = paste0(
      get_data_path("outputs", "trajectory_slide_figure", scale), ".png"
    ),
    bic_figure = paste0(get_data_path("outputs", "bic_figure", scale), ".png")
  )
}
trajectories.lst <- setNames(lapply(ready.v, trajectory_numbers.fn), ready.v)

log_section("Part 3: Pathology slopes")
slopes.res <- read_rds_safe(get_data_path("models", "pathology_slopes"))
coefficients.dt <- slopes.res$coefficients
#' One estimate with its 95% CI and p-value
#' @param model_name Model name in coefficients.dt
#' @param quantity_name Quantity in coefficients.dt
#' @return Named list: estimate, lower, upper, p, n
estimate.fn <- function(model_name, quantity_name) {
  row.dt <- coefficients.dt[model == model_name & quantity == quantity_name]
  stopifnot(nrow(row.dt) == 1)
  list(
    estimate = row.dt$estimate, lower = row.dt$lower, upper = row.dt$upper,
    p = row.dt$p, n = row.dt$n_participants
  )
}
ref_q <- "Reference slope (points per year)"
pos_q <- "Slope if positive (points per year)"
diff_q <- "Difference in slope (points per year)"
base_q <- "Difference at first assessment (points)"
participants.dt <- slopes.res$participants
pathology.lst <- list(
  participants = slopes.res$n$participants,
  assessments = slopes.res$n$assessments,
  with_tau = slopes.res$n$participants_with_tau,
  amyloid_positive = sum(participants.dt$amyloid_positive),
  tau_positive = sum(participants.dt$tau_positive, na.rm = TRUE),
  at_sizes = slopes.res$group_sizes,
  at_modelled = PATH$at_groups,
  years_to_pet_median = median(participants.dt$years_to_pet),
  pet_after_last_upsit = sum(
    participants.dt$years_to_pet > participants.dt$follow_up
  ),
  amyloid_cutoff = PET$amyloid_cutoff,
  tau_cutoff = PET$tau_cutoff,
  centiloid_step = PATH$centiloid_step,
  tau_step = PATH$tau_step,
  amyloid = list(
    negative = estimate.fn("amyloid_positive_main", ref_q),
    positive = estimate.fn("amyloid_positive_main", pos_q),
    difference = estimate.fn("amyloid_positive_main", diff_q),
    baseline = estimate.fn("amyloid_positive_main", base_q),
    sensitivity = estimate.fn("amyloid_positive_sensitivity", diff_q),
    per_step = estimate.fn("centiloid_main", diff_q)
  ),
  tau = list(
    negative = estimate.fn("tau_positive_main", ref_q),
    positive = estimate.fn("tau_positive_main", pos_q),
    difference = estimate.fn("tau_positive_main", diff_q),
    baseline = estimate.fn("tau_positive_main", base_q),
    sensitivity = estimate.fn("tau_positive_sensitivity", diff_q),
    per_step = estimate.fn("tau_suvr_main", diff_q)
  ),
  at_slopes = slopes.res$group_slopes,
  table = read_rds_safe(get_data_path("models", "pathology_description"))$table,
  figure = paste0(get_data_path("outputs", "pathology_figure"), ".png"),
  slide_figure = paste0(
    get_data_path("outputs", "pathology_slide_figure"), ".png"
  )
)

log_section("Part 4: Pathology by trajectory class")
#' Descriptive pathology table for one time scale
#' @param scale Time scale
#' @return Named list: summary and table
class_pathology.fn <- function(scale) {
  res <- read_rds_safe(get_data_path("models", "class_pathology", scale))
  list(
    summary = res$summary,
    table = res$table,
    compared = TRAJ$pathology_comparison_class[[scale]]
  )
}
class_pathology.lst <- setNames(lapply(ready.v, class_pathology.fn), ready.v)

log_section("Part 5: Agreement between time scales")
# Counts of participants by class on each scale, to show whether the two
# time scales find the same groups of people
agreement.dt <- NULL
if (all(SCALES %in% ready.v)) {
  classes.lst <- lapply(SCALES, \(scale) {
    classes.dt <- read_rds_safe(
      get_data_path("derivatives", "trajectory_classes", scale)
    )
    classes.dt[, c(SMELL$id_col, "class"), with = FALSE]
  })
  both.dt <- merge(
    classes.lst[[1]], classes.lst[[2]], by = SMELL$id_col,
    suffixes = paste0("_", SCALES)
  )
  stopifnot(nrow(both.dt) == nrow(classes.lst[[1]]))
  agreement.dt <- both.dt[, .N, by = c(paste0("class_", SCALES))]
}

log_section("Part 6: Memory and MCI by baseline UPSIT")
baseline.res <- read_rds_safe(get_data_path("models", "rbans_baseline"))
change.res <- read_rds_safe(get_data_path("models", "rbans_change"))
mci.res <- read_rds_safe(get_data_path("models", "mci_progression"))
rbans_mci.res <- read_rds_safe(
  get_data_path("models", "rbans_mci_description")
)
RBANS <- get_parameter("rbans")
PRIMARY <- "delayed_memory_index_score_60_69"
#' Estimate, CI and p of one H2 quantity
#' @param score_col RBANS score column
#' @param adjustment_name Adjustment in rbans_change
#' @param pattern Start of the quantity label
#' @return Named list: estimate, lower, upper, p, n, visits
change.fn <- function(score_col, adjustment_name, pattern) {
  model_name <- paste(score_col, adjustment_name, sep = "_")
  row.dt <- change.res$coefficients[
    model == model_name & startsWith(quantity, pattern)
  ]
  stopifnot(nrow(row.dt) == 1)
  list(
    estimate = row.dt$estimate, lower = row.dt$lower, upper = row.dt$upper,
    p = row.dt$p, n = row.dt$n_participants, visits = row.dt$n_visits
  )
}
diff_pattern <- "Difference in annual change"
scores.v <- unlist(RBANS$score_cols)
h1.dt <- baseline.res$effects
amyloid_memory.mat <- summary(
  change.res$fits[[paste0(PRIMARY, "_amyloid")]]
)$tTable
amyloid_hr.v <- summary(mci.res$fits$amyloid)$conf.int["amyloid_positive", ]
cognition.lst <- list(
  upsit_step = get_parameter("rbans_baseline")$upsit_step,
  first_assessments = baseline.res$n$first_assessments,
  with_rbans = baseline.res$n$with_rbans,
  h1 = setNames(lapply(scores.v, \(score_col) {
    h1.dt[score == score_col & adjustment == "main"]
  }), scores.v),
  h1_sensitivity = h1.dt[score == PRIMARY & adjustment == "sensitivity"],
  h2 = setNames(lapply(scores.v, \(score_col) {
    change.fn(score_col, "main", diff_pattern)
  }), scores.v),
  h2_sensitivity = change.fn(PRIMARY, "sensitivity", diff_pattern),
  h2_pet_sample = change.fn(PRIMARY, "pet_sample", diff_pattern),
  h2_amyloid = change.fn(PRIMARY, "amyloid", diff_pattern),
  memory_annual_change = change.fn(PRIMARY, "main", "Annual change"),
  memory_retest = change.fn(PRIMARY, "main", "Retest"),
  # Extra decline per year in amyloid-positive participants, H4 model
  memory_amyloid_slope = list(
    estimate = amyloid_memory.mat["years:amyloid_positive", "Value"],
    p = amyloid_memory.mat["years:amyloid_positive", "p-value"]
  ),
  rbans = change.res$n,
  h3 = setNames(
    split(mci.res$hazard_ratios, by = "adjustment", keep.by = FALSE),
    mci.res$hazard_ratios$adjustment
  ),
  mci = mci.res$n,
  amyloid_hr = list(
    estimate = amyloid_hr.v[["exp(coef)"]],
    lower = amyloid_hr.v[["lower .95"]], upper = amyloid_hr.v[["upper .95"]]
  ),
  memory_points = rbans_mci.res$memory$points,
  mci_points = rbans_mci.res$mci$points,
  mci_incidence_at_end = rbans_mci.res$mci$incidence_at_end,
  table = rbans_mci.res$table,
  memory_slide_figure = paste0(
    get_data_path("outputs", "memory_slide_figure"), ".png"
  ),
  mci_slide_figure = paste0(
    get_data_path("outputs", "mci_slide_figure"), ".png"
  ),
  memory_figure = paste0(get_data_path("outputs", "memory_figure"), ".png"),
  mci_figure = paste0(get_data_path("outputs", "mci_figure"), ".png")
)

# Correlation of the UPSIT and RBANS slopes (11), by score
coupling.res <- read_rds_safe(get_data_path("models", "slope_coupling"))
slope_term <- "years_upsit~years_rbans"
cognition.lst$coupling <- list(
  slopes = coupling.res$correlations[term == slope_term],
  participants = coupling.res$n[[1]]$participants,
  bootstrap_reps = coupling.res$config$slope_coupling$bootstrap_reps,
  slide_figure = paste0(
    get_data_path("outputs", "coupling_slide_figure"), ".png"
  )
)
# Details of the delayed memory model for the speaker notes; its time is in
# decades, so slopes are divided by trajectories.time_divisor
coupling.fit <- coupling.res$fits[[PRIMARY]]
re_cov.mat <- unclass(getVarCov(coupling.fit))
re_cor.mat <- cov2cor(re_cov.mat)
per_year <- TRAJ$time_divisor
cognition.lst$coupling$delayed <- c(
  coupling.res$n[[PRIMARY]],
  list(
    slope_cov = re_cov.mat["years_upsit", "years_rbans"] / per_year^2,
    slope_sd_upsit = sqrt(re_cov.mat["years_upsit", "years_upsit"]) /
      per_year,
    slope_sd_rbans = sqrt(re_cov.mat["years_rbans", "years_rbans"]) /
      per_year,
    intercept_cor = re_cor.mat["is_upsit", "is_rbans"],
    max_level_rate_cor = max(abs(re_cor.mat[
      c("is_upsit", "is_rbans"), c("years_upsit", "years_rbans")
    ])),
    residual_ratio = coef(
      coupling.fit$modelStruct$varStruct, unconstrained = FALSE
    )[["rbans"]],
    failed = coupling.res$correlations[
      score == PRIMARY & term == slope_term, bootstrap_failed
    ]
  )
)
cognition.lst$mci_horizon <- rbans_mci.res$mci$horizon

# For slides: the same estimates per upsit_step points lower UPSIT, the
# direction clinicians read (poorer smell); differences change sign and
# hazard ratios are inverted
lower.fn <- function(x) {
  list(estimate = -x$estimate, lower = -x$upper, upper = -x$lower, p = x$p)
}
hr_lower.fn <- function(x) {
  list(
    estimate = 1 / x$hazard_ratio, lower = 1 / x$upper, upper = 1 / x$lower,
    p = x$p, n = x$n, events = x$events
  )
}
cognition.lst$per_lower <- list(
  h1 = lapply(cognition.lst$h1, lower.fn),
  h2 = lapply(cognition.lst$h2, lower.fn),
  h2_pet_sample = lower.fn(cognition.lst$h2_pet_sample),
  h2_amyloid = lower.fn(cognition.lst$h2_amyloid),
  h3 = lapply(cognition.lst$h3, hr_lower.fn)
)

log_section("Part 7: Classes, modifiers and progression")
paths.fn <- function(key) paste0(get_data_path("outputs", key), ".png")
characteristics.res <- read_rds_safe(
  get_data_path("models", "class_characteristics")
)
class_cognition.res <- read_rds_safe(
  get_data_path("models", "class_cognition")
)
modifiers.res <- read_rds_safe(get_data_path("models", "upsit_modifiers"))
classes_extra.lst <- list(
  profile = characteristics.res$profile,
  at_groups = characteristics.res$at_groups,
  diagnoses = characteristics.res$diagnoses,
  sample = characteristics.res$sample,
  n = characteristics.res$n,
  memory = class_cognition.res$estimates,
  memory_n = class_cognition.res$n,
  profile_slide_figure = paths.fn("class_profile_slide_figure"),
  at_slide_figure = paths.fn("class_at_slide_figure"),
  diagnosis_slide_figure = paths.fn("class_diagnosis_slide_figure"),
  sample_slide_figure = paths.fn("sample_slide_figure"),
  memory_slide_figure = paths.fn("class_memory_slide_figure")
)
modifiers.lst <- list(
  modifiers = modifiers.res$modifiers,
  progression = modifiers.res$progression$slopes,
  amyloid_diff = modifiers.res$progression$amyloid_diff,
  n = modifiers.res$n,
  progression_slide_figure = paths.fn("progression_slide_figure")
)

log_section("Part 8: Save results")
env.lst <- list(
  data = data.lst,
  scales_ready = ready.v,
  scales_pending = pending.v,
  trajectories = trajectories.lst,
  pathology = pathology.lst,
  class_pathology = class_pathology.lst,
  class_agreement = agreement.dt,
  cognition = cognition.lst,
  classes_extra = classes_extra.lst,
  modifiers = modifiers.lst,
  config = list(smell = SMELL, pet = PET, pathology = PATH,
                trajectories = TRAJ),
  timestamp = Sys.time()
)
write_rds_safe(env.lst, output.path, "summary document numbers")
log_script_end("08_build_summary_env.R", success = TRUE)
