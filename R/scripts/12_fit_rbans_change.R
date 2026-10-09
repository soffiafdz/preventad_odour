#!/usr/bin/env Rscript
# =============================================================================
# 12_fit_rbans_change.R - RBANS change per year by baseline UPSIT (H2)
# =============================================================================
# Linear mixed models (nlme::lme, REML; Pinheiro & Bates 2000,
# doi:10.1007/b98882) of each RBANS index score (60-69 norms) on years since
# the first UPSIT, with a random intercept and slope per participant. lme is
# used rather than lcmm::hlme because hlme's numerical standard errors were
# inflated in the APOE model (same estimates and likelihood, 2026-10-08).
# UPSIT at the first assessment and its interaction with time give the
# difference in level and in points per year per rbans_baseline.upsit_step
# UPSIT points. Main models adjust for age at
# the first assessment, sex and education, each with its own interaction with
# time; sensitivity models add APOE e4. For H4, the main model is refitted
# on the participants with PET, without and with amyloid status (and its
# interaction with time). Every model adjusts for the test
# version and for practice: a retest indicator, 0 at a participant's first
# RBANS ever and 1 after. UPSIT and the covariates other than age are centred
# at their sample means, so the reference slope is for a participant aged 65
# with average smell and covariates. Participants and baseline are those of
# 10_fit_rbans_baseline.R; RBANS visits are those from the baseline visit on.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
#   - data/derivatives/rbans.rds: from 09_extract_rbans.R
#   - data/derivatives/pet.rds: from 04_extract_pet.R
# OUTPUTS:
#   - models/results/rbans_change.rds: fits, coefficients, sample sizes
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(nlme)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))
source(here("R/utils/baseline.R"))

log_script_start("12_fit_rbans_change.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_rbans_change", default = FALSE
)
output.path <- get_data_path("models", "rbans_change")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("12_fit_rbans_change.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
RBANS <- get_parameter("rbans")
H1 <- get_parameter("rbans_baseline")
H2 <- get_parameter("rbans_change")
AMYLOID <- get_parameter("amyloid_adjustment")$covariate
SCORE_COLS <- unlist(RBANS$score_cols)
VERSIONS <- unlist(RBANS$versions)
VERSION_COLS <- paste0("version_", VERSIONS[-1])
COVARIATES <- unlist(H2$covariates)
SENSITIVITY <- unlist(H2$sensitivity_covariates)

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
rbans.dt <- read_rds_safe(get_data_path("derivatives", "rbans"))$data
pet.dt <- read_rds_safe(get_data_path("derivatives", "pet"))$data
person.dt <- build_baseline(smell.dt, rbans.dt)
person.dt <- person.dt[, c(
  "id", "upsit_date", "rbans_date", "upsit", COVARIATES, SENSITIVITY
), with = FALSE]
amyloid.dt <- pet.dt[, .(
  id = get(get_parameter("pet")$id_col), amyloid = as.numeric(get(AMYLOID))
)]
setnames(amyloid.dt, "amyloid", AMYLOID)
person.dt <- amyloid.dt[person.dt, on = "id"]
# Kept so that figures can show UPSIT in points
upsit_mean <- mean(person.dt$upsit)
centred.v <- c(
  "upsit", setdiff(c(COVARIATES, SENSITIVITY), "age_first"), AMYLOID
)
for (col in centred.v) {
  person.dt[, (col) := get(col) - mean(get(col), na.rm = TRUE)]
}

setorderv(rbans.dt, c(RBANS$id_col, RBANS$date_col))
visit.dt <- rbans.dt[, c(
  list(
    id = get(RBANS$id_col), date = get(RBANS$date_col),
    retest = as.numeric(seq_len(.N) > 1),
    version = get(RBANS$version_col)
  ),
  .SD
), by = c(RBANS$id_col), .SDcols = SCORE_COLS]
visit.dt[, (RBANS$id_col) := NULL]
model.dt <- person.dt[visit.dt, on = "id", nomatch = NULL]
model.dt <- model.dt[date >= rbans_date]
model.dt[, `:=`(
  subject = .GRP,
  years = as.numeric(date - upsit_date) / 365.25
), by = id]
for (v in VERSIONS[-1]) {
  model.dt[, (paste0("version_", v)) := as.numeric(version == v)]
}
stopifnot(!anyNA(model.dt$version))
log_info(
  "%d participants, %d RBANS visits, follow-up median %.1f years",
  uniqueN(model.dt$id), nrow(model.dt),
  median(model.dt[, max(years), by = id]$V1)
)

#' Fit a one-class mixed model with UPSIT and covariates by time
#' @param score_col RBANS score column
#' @param covariates.v Covariates, each also interacting with time
#' @param data.df Data with the score, years, subject and predictors
#' @return lme fit
fit_change.fn <- function(score_col, covariates.v, data.df) {
  by_time.v <- c("upsit", covariates.v)
  terms.v <- c(
    "years", by_time.v, paste0("years:", by_time.v), "retest", VERSION_COLS
  )
  fixed.mod <- reformulate(terms.v, response = score_col)
  lme(
    fixed.mod, random = ~ years | subject, data = data.df, method = "REML",
    control = lmeControl(maxIter = 200, msMaxIter = 200)
  )
}

#' Estimate, 95% CI and p-value of one fixed effect
#' @param fit lme fit
#' @param term Fixed-effect name
#' @return One-row data.table
extract_term.fn <- function(fit, term) {
  coef.mat <- summary(fit)$tTable
  stopifnot(term %in% rownames(coef.mat))
  ci.mat <- intervals(fit, which = "fixed")$fixed
  data.table(
    estimate = coef.mat[term, "Value"], se = coef.mat[term, "Std.Error"],
    lower = ci.mat[term, "lower"], upper = ci.mat[term, "upper"],
    p = coef.mat[term, "p-value"]
  )
}

log_section("Part 2: One model per score")
adjustment.lst <- list(
  main = COVARIATES, sensitivity = c(COVARIATES, SENSITIVITY),
  pet_sample = COVARIATES, amyloid = c(COVARIATES, AMYLOID)
)
# Columns that must be present: pet_sample uses the amyloid model's sample
required.lst <- modifyList(
  adjustment.lst, list(pet_sample = c(COVARIATES, AMYLOID))
)
grid.dt <- CJ(score = SCORE_COLS, adjustment = names(adjustment.lst),
              sorted = FALSE)
quantities.v <- c(
  years = "Annual change at average UPSIT (points per year)",
  upsit = sprintf("Difference at baseline per %g UPSIT points (points)",
                  H1$upsit_step),
  `years:upsit` = sprintf(
    "Difference in annual change per %g UPSIT points (points per year)",
    H1$upsit_step
  ),
  retest = "Retest effect (points)"
)

fits.lst <- list()
coefficients.lst <- list()
for (i in seq_len(nrow(grid.dt))) {
  spec <- grid.dt[i]
  name <- paste(spec$score, spec$adjustment, sep = "_")
  covariates.v <- adjustment.lst[[spec$adjustment]]
  required.v <- c(spec$score, required.lst[[spec$adjustment]])
  keep.v <- complete.cases(model.dt[, required.v, with = FALSE])
  score.df <- as.data.frame(model.dt[keep.v])
  fit <- fit_change.fn(spec$score, covariates.v, score.df)
  fits.lst[[name]] <- fit
  coefficients.lst[[name]] <- rbindlist(lapply(names(quantities.v), \(q) {
    cbind(quantity = quantities.v[[q]], extract_term.fn(fit, q))
  }))[, `:=`(
    model = name, score = spec$score, adjustment = spec$adjustment,
    n_participants = length(unique(score.df$subject)),
    n_visits = nrow(score.df)
  )]
  change.v <- coefficients.lst[[name]][3]
  log_info(
    "%s: %.2f points/year per %g UPSIT points (95%% CI %.2f to %.2f)",
    name, change.v$estimate, H1$upsit_step, change.v$lower, change.v$upper
  )
}
coefficients.dt <- rbindlist(coefficients.lst)
setcolorder(coefficients.dt, c("model", "score", "adjustment", "quantity"))

log_section("Part 3: Save results")
results.lst <- list(
  fits = fits.lst,
  coefficients = coefficients.dt,
  n = list(
    participants = uniqueN(model.dt$id),
    visits = nrow(model.dt),
    follow_up_median = median(model.dt[, max(years), by = id]$V1)
  ),
  # Sample mean of baseline UPSIT, in steps of rbans_baseline.upsit_step,
  # subtracted before fitting
  upsit_mean = upsit_mean,
  config = list(rbans_change = H2, rbans_baseline = H1, rbans = RBANS),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "RBANS change by baseline UPSIT")
log_script_end("12_fit_rbans_change.R", success = TRUE)
