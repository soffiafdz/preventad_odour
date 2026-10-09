#!/usr/bin/env Rscript
# =============================================================================
# 13_fit_mci_progression.R - Progression to MCI by baseline UPSIT (H3)
# =============================================================================
# Cox proportional hazards models (survival::coxph; Therneau & Grambsch
# 2000, doi:10.1007/978-1-4757-3294-8) of time from the first UPSIT to an
# MCI diagnosis, by UPSIT at the first assessment, per
# rbans_baseline.upsit_step points. The event is the first visit with an
# MCI diagnosis in the MCI review export; participants not in the export
# were never diagnosed, and those whose MCI reverted to normal are
# excluded. Follow-up for them ends at the earlier of their last
# RBANS visit with a score and the latest review in the export, after which
# diagnoses are not recorded. Main models adjust for age at the first
# assessment, sex and education; sensitivity models add APOE e4. For H4,
# the main model is refitted on the participants with PET, without and with
# amyloid status. Participants and baseline are those of
# 10_fit_rbans_baseline.R. Diagnoses are made at study visits, so event
# times are only known to the visit.
# Proportional hazards are checked with scaled Schoenfeld residuals.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
#   - data/derivatives/rbans.rds: from 09_extract_rbans.R
#   - data/derivatives/pet.rds: from 04_extract_pet.R
#   - data/raw/: MCI review export named in the config
# OUTPUTS:
#   - models/results/mci_progression.rds: fits, hazard ratios, checks
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(survival)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))
source(here("R/utils/baseline.R"))

log_script_start("13_fit_mci_progression.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_mci_progression", default = FALSE
)
output.path <- get_data_path("models", "mci_progression")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("13_fit_mci_progression.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
RBANS <- get_parameter("rbans")
MCI <- get_parameter("mci")
H1 <- get_parameter("rbans_baseline")
H3 <- get_parameter("mci_progression")
COVARIATES <- unlist(H3$covariates)
SENSITIVITY <- unlist(H3$sensitivity_covariates)
AMYLOID <- get_parameter("amyloid_adjustment")$covariate
KEEP_COLS <- c(
  MCI$id_col, MCI$diagnosis_date_col, MCI$review_date_col, MCI$subtype_col
)
DATE_PATTERN <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
rbans.dt <- read_rds_safe(get_data_path("derivatives", "rbans"))$data
pet.dt <- read_rds_safe(get_data_path("derivatives", "pet"))$data
mci.dt <- read_csv_safe(
  get_data_path("raw", "mci_csv"),
  select = KEEP_COLS,
  # Read as text so IDs keep leading zeros and malformed dates stay visible
  colClasses = list(character = KEEP_COLS)
)
validate_columns(mci.dt, KEEP_COLS, "MCI export")
validate_not_empty(mci.dt, "MCI export")
stopifnot(
  all(grepl(DATE_PATTERN, mci.dt[[MCI$diagnosis_date_col]])),
  all(grepl(DATE_PATTERN, mci.dt[[MCI$review_date_col]]))
)
coverage_end <- max(as.IDate(mci.dt[[MCI$review_date_col]]))
reverted.v <- unique(mci.dt[
  get(MCI$subtype_col) == as.character(MCI$reverted_value), get(MCI$id_col)
])
log_info("Latest MCI review: %s", format(coverage_end))
# One row per review; the diagnosis date repeats on each
mci.dt <- unique(mci.dt[, .(
  id = get(MCI$id_col), mci_date = as.IDate(get(MCI$diagnosis_date_col))
)])
validate_unique_ids(mci.dt, "id")

log_section("Part 2: Follow-up")
person.dt <- build_baseline(smell.dt, rbans.dt)
last.dt <- rbans.dt[, .(last_rbans = max(get(RBANS$date_col))),
                    by = .(id = get(RBANS$id_col))]
model.dt <- mci.dt[person.dt, on = "id"]
model.dt <- last.dt[model.dt, on = "id"]
amyloid.dt <- pet.dt[, .(
  id = get(get_parameter("pet")$id_col), amyloid = as.numeric(get(AMYLOID))
)]
setnames(amyloid.dt, "amyloid", AMYLOID)
model.dt <- amyloid.dt[model.dt, on = "id"]
model.dt[, mci := as.numeric(!is.na(mci_date))]
log_info(
  "Participants whose MCI reverted to normal, excluded: %d",
  sum(model.dt$id %in% reverted.v)
)
model.dt <- model.dt[!id %in% reverted.v]
log_info(
  "Participants with MCI in the export: %d, of whom with a baseline: %d",
  nrow(mci.dt), sum(model.dt$mci)
)
model.dt[, end_date := fifelse(
  mci == 1, mci_date, pmin(last_rbans, coverage_end)
)]
model.dt[, years := as.numeric(end_date - upsit_date) / 365.25]
stopifnot(model.dt[mci == 1, all(years > 0)])
no_follow_up.v <- model.dt$years <= 0
log_info("Participants without follow-up, dropped: %d", sum(no_follow_up.v))
model.dt <- model.dt[!no_follow_up.v]
log_info(
  "%d participants, %d with MCI, follow-up median %.1f years",
  nrow(model.dt), sum(model.dt$mci), median(model.dt$years)
)

log_section("Part 3: One model per adjustment")
adjustment.lst <- list(
  main = COVARIATES, sensitivity = c(COVARIATES, SENSITIVITY),
  pet_sample = COVARIATES, amyloid = c(COVARIATES, AMYLOID)
)
# Columns that must be present: pet_sample uses the amyloid model's sample
required.lst <- modifyList(
  adjustment.lst, list(pet_sample = c(COVARIATES, AMYLOID))
)
fits.lst <- list()
ph.lst <- list()
hazard.lst <- list()
for (adjustment in names(adjustment.lst)) {
  formula.mod <- reformulate(
    c("upsit", adjustment.lst[[adjustment]]),
    response = quote(Surv(years, mci))
  )
  keep.v <- complete.cases(
    model.dt[, required.lst[[adjustment]], with = FALSE]
  )
  # The model frame is kept for predicted curves in 14
  fit <- coxph(formula.mod, data = model.dt[keep.v], model = TRUE)
  fits.lst[[adjustment]] <- fit
  ph.lst[[adjustment]] <- cox.zph(fit)
  ci.v <- exp(confint(fit)["upsit", ])
  hazard.lst[[adjustment]] <- data.table(
    adjustment = adjustment,
    hazard_ratio = exp(coef(fit)[["upsit"]]),
    lower = ci.v[[1]], upper = ci.v[[2]],
    p = summary(fit)$coefficients["upsit", "Pr(>|z|)"],
    n = fit$n, events = fit$nevent,
    ph_p_upsit = ph.lst[[adjustment]]$table["upsit", "p"],
    ph_p_global = ph.lst[[adjustment]]$table["GLOBAL", "p"]
  )
  log_info(
    "%s: HR %.2f per %g UPSIT points (95%% CI %.2f to %.2f), %d events",
    adjustment, hazard.lst[[adjustment]]$hazard_ratio, H1$upsit_step,
    ci.v[[1]], ci.v[[2]], fit$nevent
  )
}
hazard.dt <- rbindlist(hazard.lst)

log_section("Part 4: Save results")
results.lst <- list(
  fits = fits.lst,
  hazard_ratios = hazard.dt,
  proportional_hazards = ph.lst,
  n = list(
    with_baseline = nrow(person.dt),
    mci_in_export = nrow(mci.dt),
    reverted = length(reverted.v),
    coverage_end = coverage_end,
    participants = nrow(model.dt),
    events = sum(model.dt$mci),
    dropped_no_follow_up = sum(no_follow_up.v),
    follow_up_median = median(model.dt$years)
  ),
  config = list(mci = MCI, mci_progression = H3, rbans_baseline = H1),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "MCI progression by UPSIT")
log_script_end("13_fit_mci_progression.R", success = TRUE)
