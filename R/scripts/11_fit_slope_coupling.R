#!/usr/bin/env Rscript
# =============================================================================
# 11_fit_slope_coupling.R - Correlation of UPSIT and RBANS slopes (H5)
# =============================================================================
# Bivariate linear mixed model (nlme::lme; Pinheiro & Bates 2000,
# doi:10.1007/b98882) of UPSIT and one RBANS index score (60-69 norms),
# stacked in long format. Each outcome has its own intercept, slope on time
# since the participant's first UPSIT (in decades), and covariates (age at
# the first assessment, sex, education); RBANS also has its test version and
# a retest indicator (0 at the participant's first RBANS, 1 after) for
# practice effects. Each participant has a random intercept and slope per
# outcome with an unstructured covariance matrix, and each outcome its own
# residual variance. The estimate of interest is the correlation of the two
# random slopes: whether people whose smell declines faster also decline
# faster on the RBANS score. Its 95% interval comes from a bootstrap of
# participants (percentile intervals). UPSIT assessments are those of the
# trajectory models; RBANS uses every visit of the same participants.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
#   - data/derivatives/rbans.rds: from 09_extract_rbans.R
# OUTPUTS:
#   - models/results/slope_coupling.rds: fits, random-effect correlations
#     with bootstrap intervals, bootstrap replicates
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

log_script_start("11_fit_slope_coupling.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_slope_coupling", default = FALSE
)
output.path <- get_data_path("models", "slope_coupling")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("11_fit_slope_coupling.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
RBANS <- get_parameter("rbans")
TRAJ <- get_parameter("trajectories")
H5 <- get_parameter("slope_coupling")
ID <- SMELL$id_col
SCORE_COLS <- unlist(RBANS$score_cols)
COVARIATES <- unlist(H5$covariates)
VERSIONS <- unlist(RBANS$versions)

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
rbans.dt <- read_rds_safe(get_data_path("derivatives", "rbans"))$data
# Same assessments as the trajectory models
smell.dt <- smell.dt[!get(TRAJ$exclude_col) %in% TRAJ$exclude_values]
smell.dt[, n_assessments := .N, by = ID]
smell.dt <- smell.dt[n_assessments >= TRAJ$min_assessments]

person.dt <- smell.dt[, .(
  first_date = min(get(SMELL$date_col)),
  age_first = (min(get(SMELL$age_col)) - TRAJ$age_centre) /
    TRAJ$time_divisor,
  female = as.numeric(get(SMELL$sex_col)[1] == SMELL$female_value),
  education = as.numeric(get(SMELL$education_col)[1])
), by = .(id = get(ID))]
upsit.dt <- smell.dt[, .(
  id = get(ID), date = get(SMELL$date_col), y = get(SMELL$score_col)
)]

rbans.dt <- rbans.dt[get(RBANS$id_col) %in% person.dt$id]
setorderv(rbans.dt, c(RBANS$id_col, RBANS$date_col))
rbans.dt[, retest := as.numeric(seq_len(.N) > 1), by = c(RBANS$id_col)]
log_info(
  "%d participants, %d with RBANS",
  nrow(person.dt), uniqueN(rbans.dt[[RBANS$id_col]])
)

#' Stack UPSIT and one RBANS score in long format
#' @param score_col RBANS score column
#' @return data.frame with outcome indicators, time and covariates
stack_outcomes.fn <- function(score_col) {
  memory.dt <- rbans.dt[!is.na(get(score_col)), .(
    id = get(RBANS$id_col), date = get(RBANS$date_col),
    y = get(score_col), retest, version = get(RBANS$version_col)
  )]
  long.dt <- rbindlist(list(
    upsit.dt[, `:=`(outcome = "upsit", retest = 0, version = NA_character_)],
    memory.dt[, outcome := "rbans"]
  ), use.names = TRUE)
  long.dt <- person.dt[long.dt, on = "id"]
  long.dt[, `:=`(
    outcome = factor(outcome, levels = c("upsit", "rbans")),
    is_upsit = as.numeric(outcome == "upsit"),
    is_rbans = as.numeric(outcome == "rbans"),
    # In decades, as in the trajectory models, which keeps the slope and
    # intercept variances on similar scales. Correlations do not depend on
    # the unit.
    years = as.numeric(date - first_date) / 365.25 / TRAJ$time_divisor
  )]
  long.dt[, `:=`(
    years_upsit = is_upsit * years, years_rbans = is_rbans * years
  )]
  for (v in VERSIONS[-1]) {
    long.dt[, (paste0("version_", v)) := as.numeric(
      outcome == "rbans" & version == v
    )]
  }
  as.data.frame(long.dt)
}

#' Fit the bivariate model
#' @param long.df Output of stack_outcomes.fn
#' @return lme fit
fit_coupling.fn <- function(long.df) {
  fixed.v <- c(
    "outcome", paste0("outcome:", c("years", COVARIATES)),
    "retest", paste0("version_", VERSIONS[-1])
  )
  fixed.mod <- reformulate(c("0", fixed.v), response = "y")
  lme(
    fixed.mod,
    random = list(id = pdSymm(
      ~ 0 + is_upsit + years_upsit + is_rbans + years_rbans
    )),
    weights = varIdent(form = ~ 1 | outcome),
    data = long.df, method = "REML",
    control = lmeControl(maxIter = 200, msMaxIter = 200)
  )
}

#' Correlations between the random effects of a fit
#' @param fit lme fit
#' @return Named vector, one element per pair of random effects
re_correlations.fn <- function(fit) {
  cor.mat <- cov2cor(unclass(getVarCov(fit)))
  pairs.mat <- which(lower.tri(cor.mat), arr.ind = TRUE)
  setNames(
    cor.mat[pairs.mat],
    paste(colnames(cor.mat)[pairs.mat[, 2]],
          rownames(cor.mat)[pairs.mat[, 1]], sep = "~")
  )
}

#' Refit on participants resampled with replacement
#' @param long.df Output of stack_outcomes.fn
#' @param reps Number of bootstrap samples
#' @param terms.v Names from re_correlations.fn
#' @return Matrix with one row per sample (NA where the fit failed)
bootstrap_correlations.fn <- function(long.df, reps, terms.v) {
  rows.lst <- split(seq_len(nrow(long.df)), long.df$id)
  # Reproducible parallel draws: one L'Ecuyer-CMRG stream per refit
  RNGkind("L'Ecuyer-CMRG")
  set.seed(get_seed())
  cor.lst <- parallel::mclapply(seq_len(reps), \(b) {
    draw.lst <- rows.lst[sample(length(rows.lst), replace = TRUE)]
    boot.df <- long.df[unlist(draw.lst), ]
    # Each draw is its own participant, even when drawn twice
    boot.df$id <- rep(seq_along(draw.lst), lengths(draw.lst))
    tryCatch(
      re_correlations.fn(fit_coupling.fn(boot.df))[terms.v],
      error = function(e) rep(NA_real_, length(terms.v))
    )
  }, mc.cores = H5$bootstrap_cores, mc.set.seed = TRUE)
  boot.mat <- do.call(rbind, cor.lst)
  colnames(boot.mat) <- terms.v
  boot.mat
}

log_section("Part 2: One model per score")
fits.lst <- list()
correlations.lst <- list()
bootstrap.lst <- list()
n.lst <- list()
for (score in SCORE_COLS) {
  long.df <- stack_outcomes.fn(score)
  fit <- fit_coupling.fn(long.df)
  fits.lst[[score]] <- fit
  estimate.v <- re_correlations.fn(fit)
  boot.mat <- bootstrap_correlations.fn(
    long.df, H5$bootstrap_reps, names(estimate.v)
  )
  bootstrap.lst[[score]] <- boot.mat
  failed <- sum(is.na(boot.mat[, 1]))
  correlations.lst[[score]] <- data.table(
    score = score, term = names(estimate.v), estimate = estimate.v,
    lower = apply(boot.mat, 2, quantile, 0.025, na.rm = TRUE),
    upper = apply(boot.mat, 2, quantile, 0.975, na.rm = TRUE),
    bootstrap_failed = failed
  )
  n.lst[[score]] <- list(
    participants = length(unique(long.df$id)),
    upsit_assessments = sum(long.df$is_upsit),
    rbans_visits = sum(long.df$is_rbans),
    rbans_before_first_upsit = sum(long.df$is_rbans & long.df$years < 0)
  )
  slope.dt <- correlations.lst[[score]][term == "years_upsit~years_rbans"]
  log_info(
    "%s: slope correlation %.2f (95%% CI %.2f to %.2f), %d failed fits",
    score, slope.dt$estimate, slope.dt$lower, slope.dt$upper, failed
  )
}
correlations.dt <- rbindlist(correlations.lst)

log_section("Part 3: Save results")
results.lst <- list(
  fits = fits.lst,
  correlations = correlations.dt,
  bootstrap = bootstrap.lst,
  n = n.lst,
  config = list(slope_coupling = H5, rbans = RBANS),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "UPSIT and RBANS slope coupling")
log_script_end("11_fit_slope_coupling.R", success = TRUE)
