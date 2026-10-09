#!/usr/bin/env Rscript
# =============================================================================
# 09_extract_rbans.R - Extract the RBANS assessments
# =============================================================================
# Keeps every RBANS visit from the RBANS export: participant ID, date, age,
# visit label, test version, and the total scale, immediate memory and
# delayed memory index scores from the 60-69 norms. Visits without any of the
# three scores are dropped and counted; a visit missing only some keeps the
# others.
# Visits are not matched to UPSIT assessments here; the analyses do that.
#
# INPUTS:
#   - data/raw/: RBANS export named in the config
# OUTPUTS:
#   - data/derivatives/rbans.rds: one row per RBANS visit
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))

log_script_start("09_extract_rbans.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "extract_rbans", default = FALSE
)
output.path <- get_data_path("derivatives", "rbans")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("09_extract_rbans.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
RBANS <- get_parameter("rbans")
SCORE_COLS <- unlist(RBANS$score_cols)
KEEP_COLS <- c(
  RBANS$id_col, RBANS$date_col, RBANS$raw_age_col, RBANS$visit_col,
  RBANS$version_col, SCORE_COLS
)

log_section("Part 1: Load data")
raw.path <- get_data_path("raw", "rbans_csv")
rbans.dt <- read_csv_safe(
  raw.path,
  select = KEEP_COLS,
  # Read as text so IDs with leading zeros keep them
  colClasses = list(character = RBANS$id_col)
)
validate_columns(rbans.dt, KEEP_COLS, "RBANS export")
validate_not_empty(rbans.dt, "RBANS export")

log_section("Part 2: Visits")
unscored.v <- rowSums(!is.na(rbans.dt[, ..SCORE_COLS])) == 0
log_info("Visits without any of the scores, dropped: %d", sum(unscored.v))
rbans.dt <- rbans.dt[!unscored.v]
stopifnot(!anyNA(rbans.dt[[RBANS$date_col]]))
validate_unique_ids(rbans.dt, c(RBANS$id_col, RBANS$date_col))
validate_categorical(rbans.dt, RBANS$version_col, unlist(RBANS$versions))
log_info(
  "Visits without a test version, kept: %d",
  sum(is.na(rbans.dt[[RBANS$version_col]]))
)
# Index scores are standard scores, so positive
for (col in SCORE_COLS) {
  validate_num_range(rbans.dt, col, 0, Inf, na_allowed = TRUE)
}

# Candidate_Age is in months: divided by 12, its change between visits
# matches days / 365.25 to within 0.014 years
rbans.dt[, (RBANS$age_col) := get(RBANS$raw_age_col) / 12]
rbans.dt[, (RBANS$raw_age_col) := NULL]
setorderv(rbans.dt, c(RBANS$id_col, RBANS$date_col))
log_info(
  "%d visits from %d participants",
  nrow(rbans.dt), uniqueN(rbans.dt[[RBANS$id_col]])
)

log_section("Part 3: Save results")
results.lst <- list(
  data = rbans.dt,
  n = list(
    visits = nrow(rbans.dt),
    participants = uniqueN(rbans.dt[[RBANS$id_col]]),
    missing = vapply(
      SCORE_COLS, function(col) sum(is.na(rbans.dt[[col]])), integer(1)
    ),
    missing_version = sum(is.na(rbans.dt[[RBANS$version_col]])),
    dropped_unscored = sum(unscored.v)
  ),
  config = list(source = basename(raw.path), rbans = RBANS),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "extracted RBANS data")
log_script_end("09_extract_rbans.R", success = TRUE)
