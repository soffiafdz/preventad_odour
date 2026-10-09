#!/usr/bin/env Rscript
# =============================================================================
# 10_fit_rbans_baseline.R - RBANS by UPSIT at the first assessment (H1)
# =============================================================================
# Linear regression of each RBANS index score (60-69 norms) on the UPSIT
# total score at each participant's first assessment, using the RBANS from
# the same visit (Study_visit_label). The first assessment follows the same
# visit exclusions as the trajectory models. Main models adjust for age at
# the first assessment, sex and education; sensitivity models add APOE e4.
# The UPSIT effect is given per rbans_baseline.upsit_step points, so a
# positive estimate means better smell goes with higher index scores.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
#   - data/derivatives/rbans.rds: from 09_extract_rbans.R
# OUTPUTS:
#   - models/results/rbans_baseline.rds: fits, UPSIT effects, sample sizes
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))
source(here("R/utils/baseline.R"))

log_script_start("10_fit_rbans_baseline.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_rbans_baseline", default = FALSE
)
output.path <- get_data_path("models", "rbans_baseline")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("10_fit_rbans_baseline.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
RBANS <- get_parameter("rbans")
H1 <- get_parameter("rbans_baseline")
SCORE_COLS <- unlist(RBANS$score_cols)

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
rbans.dt <- read_rds_safe(get_data_path("derivatives", "rbans"))$data
model.dt <- build_baseline(smell.dt, rbans.dt)

log_section("Part 2: One model per score")
adjustment.lst <- list(
  main = unlist(H1$covariates),
  sensitivity = c(unlist(H1$covariates), unlist(H1$sensitivity_covariates))
)
grid.dt <- CJ(score = SCORE_COLS, adjustment = names(adjustment.lst),
              sorted = FALSE)

fits.lst <- list()
effects.lst <- list()
for (i in seq_len(nrow(grid.dt))) {
  spec <- grid.dt[i]
  name <- paste(spec$score, spec$adjustment, sep = "_")
  formula.mod <- reformulate(
    c("upsit", adjustment.lst[[spec$adjustment]]), response = spec$score
  )
  fit <- lm(formula.mod, data = model.dt)
  fits.lst[[name]] <- fit
  ci.v <- confint(fit)["upsit", ]
  effects.lst[[name]] <- data.table(
    model = name, score = spec$score, adjustment = spec$adjustment,
    estimate = unname(coef(fit)["upsit"]),
    lower = unname(ci.v[1]), upper = unname(ci.v[2]),
    p = summary(fit)$coefficients["upsit", "Pr(>|t|)"],
    n = nobs(fit)
  )
  log_info(
    "%s: %.2f points per %g UPSIT points (95%% CI %.2f to %.2f), n = %d",
    name, effects.lst[[name]]$estimate, H1$upsit_step,
    effects.lst[[name]]$lower, effects.lst[[name]]$upper, nobs(fit)
  )
}
effects.dt <- rbindlist(effects.lst)

log_section("Part 3: Save results")
results.lst <- list(
  fits = fits.lst,
  effects = effects.dt,
  n = list(
    first_assessments = attr(model.dt, "n_first"),
    with_rbans = nrow(model.dt),
    max_gap_days = max(
      abs(as.numeric(model.dt$rbans_date - model.dt$upsit_date))
    )
  ),
  config = list(rbans_baseline = H1, rbans = RBANS),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "RBANS by baseline UPSIT")
log_script_end("10_fit_rbans_baseline.R", success = TRUE)
