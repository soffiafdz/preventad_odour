#!/usr/bin/env Rscript
# =============================================================================
# 05_fit_pathology_slopes.R - UPSIT change per year by amyloid and tau
# =============================================================================
# Linear mixed models (lcmm::hlme, one class; Proust-Lima, Philipps & Liquet
# 2017, J Stat Softw 78(2), doi:10.18637/jss.v078.i02) of UPSIT total score
# on years since the first assessment, with a random intercept and slope per
# participant. One model per PET marker (first scan): the marker and its
# interaction with time give the difference in level and in points per year.
# Main models adjust for age at the first assessment and sex, each with its
# own interaction with time; sensitivity models add APOE e4 and education.
# Covariates other than age are centred at their sample means, so reference
# slopes are for a participant aged 65 with average covariates. Each model
# uses the participants with that marker (one first scan has no tau). The
# A/T groups listed in pathology.at_groups get their own slopes.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
#   - data/derivatives/pet.rds: from 04_extract_pet.R
# OUTPUTS:
#   - models/results/pathology_slopes.rds: fits, coefficients, group slopes
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(lcmm)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))

log_script_start("05_fit_pathology_slopes.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_pathology_slopes", default = FALSE
)
output.path <- get_data_path("models", "pathology_slopes")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("05_fit_pathology_slopes.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
PET <- get_parameter("pet")
TRAJ <- get_parameter("trajectories")
PATH <- get_parameter("pathology")
ID <- SMELL$id_col

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
pet.dt <- read_rds_safe(get_data_path("derivatives", "pet"))$data
# Same assessments as the trajectory models
smell.dt <- smell.dt[!get(TRAJ$exclude_col) %in% TRAJ$exclude_values]
smell.dt[, n_assessments := .N, by = ID]
smell.dt <- smell.dt[n_assessments >= TRAJ$min_assessments]
model.dt <- pet.dt[smell.dt, on = ID, nomatch = NULL]
validate_not_empty(model.dt, "UPSIT assessments with PET")

model.dt[, `:=`(
  subject = .GRP,
  score = get(SMELL$score_col),
  years = as.numeric(get(SMELL$date_col) - min(get(SMELL$date_col))) / 365.25,
  age_first = (min(get(SMELL$age_col)) - TRAJ$age_centre) / TRAJ$time_divisor,
  female = as.numeric(get(SMELL$sex_col)[1] == SMELL$female_value),
  apoe_carrier = as.numeric(get(SMELL$apoe_col)[1] == SMELL$apoe_carrier_value),
  education = as.numeric(get(SMELL$education_col)[1])
), by = ID]
person.dt <- model.dt[, .SD[1], by = ID]
for (col in c("female", "apoe_carrier", "education")) {
  model.dt[, (col) := get(col) - mean(person.dt[[col]])]
}
# fcase() would treat missing tau as negative, so it is caught first
model.dt[, at_group := fcase(
  is.na(tau_positive), "tau missing",
  amyloid_positive & tau_positive, "A+T+",
  amyloid_positive, "A+T-",
  tau_positive, "A-T+",
  default = "A-T-"
)]
model.dt[, `:=`(
  amyloid_positive = as.numeric(amyloid_positive),
  tau_positive = as.numeric(tau_positive),
  centiloid = (centiloid - PET$amyloid_cutoff) / PATH$centiloid_step,
  tau_suvr = (tau_suvr - PET$tau_cutoff) / PATH$tau_step
)]
at_dummy.v <- c(
  `A-T-` = "at_neg_neg", `A+T-` = "at_pos_neg",
  `A-T+` = "at_neg_pos", `A+T+` = "at_pos_pos"
)
for (g in PATH$at_groups[-1]) {
  model.dt[, (at_dummy.v[[g]]) := as.numeric(at_group == g)]
}
log_info(
  "%d participants, %d assessments",
  uniqueN(model.dt$subject), nrow(model.dt)
)

marker_labels.v <- c(
  amyloid_positive = sprintf(
    "Amyloid positive (Centiloid > %g)", PET$amyloid_cutoff
  ),
  tau_positive = sprintf("Tau positive (meta-ROI > %g)", PET$tau_cutoff),
  centiloid = sprintf(
    "Centiloid, per %g above %g", PATH$centiloid_step, PET$amyloid_cutoff
  ),
  tau_suvr = sprintf(
    "Tau meta-ROI, per %g SUVR above %g", PATH$tau_step, PET$tau_cutoff
  )
)

#' Fit a one-class mixed model with every predictor interacting with time
#' @param predictors.v Predictor columns of the data
#' @param data.df Data with score, years and subject
#' @return hlme fit
fit_slopes.fn <- function(predictors.v, data.df) {
  terms.v <- c("years", predictors.v, paste0("years:", predictors.v))
  fixed.mod <- reformulate(terms.v, response = "score")
  fit <- eval(bquote(hlme(
    .(fixed.mod), random = ~ years, subject = "subject", ng = 1,
    data = data.df, verbose = FALSE
  )))
  stopifnot(fit$conv == 1)
  fit
}

#' Estimate, 95% CI and p-value of a sum of fixed effects
#' @param fit hlme fit
#' @param terms.v Fixed-effect names to add up
#' @return One-row data.table
combine_terms.fn <- function(fit, terms.v) {
  vcov.mat <- VarCov(fit)
  idx.v <- match(terms.v, names(fit$best))
  stopifnot(!anyNA(idx.v))
  est <- sum(fit$best[idx.v])
  se <- sqrt(sum(vcov.mat[idx.v, idx.v]))
  data.table(
    estimate = est, se = se,
    lower = est - 1.96 * se, upper = est + 1.96 * se,
    p = 2 * pnorm(-abs(est / se))
  )
}

log_section("Part 2: One model per marker")
model.df <- as.data.frame(model.dt)
adjustment.lst <- list(
  main = PATH$covariates,
  sensitivity = c(PATH$covariates, PATH$sensitivity_covariates)
)
grid.dt <- CJ(marker = PATH$markers, adjustment = names(adjustment.lst),
              sorted = FALSE)

fits.lst <- list()
coefficients.lst <- list()
for (i in seq_len(nrow(grid.dt))) {
  spec <- grid.dt[i]
  name <- paste(spec$marker, spec$adjustment, sep = "_")
  marker.df <- model.df[!is.na(model.df[[spec$marker]]), ]
  fit <- fit_slopes.fn(
    c(spec$marker, adjustment.lst[[spec$adjustment]]), marker.df
  )
  fits.lst[[name]] <- fit
  # For a binary marker, the reference slope is the negative group's; for a
  # continuous one, the slope at the cutoff
  coefficients.lst[[name]] <- rbind(
    cbind(quantity = "Reference slope (points per year)",
          combine_terms.fn(fit, "years")),
    cbind(quantity = "Difference at first assessment (points)",
          combine_terms.fn(fit, spec$marker)),
    cbind(quantity = "Difference in slope (points per year)",
          combine_terms.fn(fit, paste0("years:", spec$marker))),
    if (spec$marker %in% c("amyloid_positive", "tau_positive")) {
      cbind(quantity = "Slope if positive (points per year)",
            combine_terms.fn(fit, c("years", paste0("years:", spec$marker))))
    }
  )[, `:=`(
    model = name, marker = spec$marker,
    marker_label = marker_labels.v[[spec$marker]],
    adjustment = spec$adjustment,
    n_participants = uniqueN(marker.df$subject),
    n_assessments = nrow(marker.df)
  )]
  log_info(
    "%s: slope difference %.2f points/year (95%% CI %.2f to %.2f)",
    name, coefficients.lst[[name]][3, estimate],
    coefficients.lst[[name]][3, lower], coefficients.lst[[name]][3, upper]
  )
}
coefficients.dt <- rbindlist(coefficients.lst)
setcolorder(coefficients.dt, c("model", "marker", "marker_label",
                               "adjustment", "quantity"))

log_section("Part 3: A/T group slopes")
at.df <- as.data.frame(model.dt[at_group %in% PATH$at_groups])
at.fit <- fit_slopes.fn(
  c(unname(at_dummy.v[PATH$at_groups[-1]]), PATH$covariates), at.df
)
fits.lst[["at_groups_main"]] <- at.fit
# The first group is the reference; the others add their difference
group_slopes.dt <- rbindlist(lapply(PATH$at_groups, \(g) {
  terms.v <- "years"
  if (g != PATH$at_groups[1]) {
    terms.v <- c(terms.v, paste0("years:", at_dummy.v[[g]]))
  }
  cbind(group = g, combine_terms.fn(at.fit, terms.v))
}))
participants.dt <- model.dt[, .(
  amyloid_positive = amyloid_positive[1] == 1,
  tau_positive = tau_positive[1] == 1,
  at_group = at_group[1],
  follow_up = max(years),
  years_to_pet = as.numeric(pet_date[1] - min(get(SMELL$date_col))) / 365.25
), by = subject]
group_n.dt <- participants.dt[, .N, by = .(group = at_group)]
group_slopes.dt <- group_n.dt[group_slopes.dt, on = "group"]
setnames(group_slopes.dt, "N", "participants")
log_info(
  "Participants outside the A/T group model: %d",
  participants.dt[!at_group %in% PATH$at_groups, .N]
)

log_section("Part 4: Save results")
results.lst <- list(
  fits = fits.lst,
  coefficients = coefficients.dt,
  group_slopes = group_slopes.dt,
  group_sizes = group_n.dt,
  at_dummies = at_dummy.v[PATH$at_groups[-1]],
  participants = participants.dt,
  n = list(
    participants = uniqueN(model.dt$subject),
    assessments = nrow(model.dt),
    participants_with_tau = participants.dt[!is.na(tau_positive), .N]
  ),
  config = list(pet = PET, pathology = PATH, trajectories = TRAJ),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "pathology slope models")
log_script_end("05_fit_pathology_slopes.R", success = TRUE)
