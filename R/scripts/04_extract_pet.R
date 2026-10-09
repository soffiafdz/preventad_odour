#!/usr/bin/env Rscript
# =============================================================================
# 04_extract_pet.R - Extract each participant's first PET scan
# =============================================================================
# Keeps, from the PET extract, the first scan per participant: scan date,
# amyloid in Centiloids, tau meta-ROI SUVR, and the amyloid and tau
# positivity statuses. Later scans are left out because most models use one
# pathology measure per person. Scans without a valid date are dropped and
# counted. A first scan without tau is kept, with tau missing, so the
# participant still counts for amyloid. The statuses are checked against the
# provider's cutoffs.
#
# INPUTS:
#   - data/raw/: PET extract named in the config
# OUTPUTS:
#   - data/derivatives/pet.rds: one row per participant
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))

log_script_start("04_extract_pet.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "extract_pet", default = FALSE
)
output.path <- get_data_path("derivatives", "pet")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("04_extract_pet.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
PET <- get_parameter("pet")
KEEP_COLS <- c(
  PET$id_col, PET$session_col, PET$date_col, PET$amyloid_col, PET$tau_col,
  PET$amyloid_status_col, PET$tau_status_col
)

log_section("Part 1: Load data")
raw.path <- get_data_path("raw", "pet_csv")
pet.dt <- read_csv_safe(
  raw.path,
  select = KEEP_COLS,
  # Read as text so IDs keep leading zeros and placeholder dates stay visible
  colClasses = list(character = c(PET$id_col, PET$date_col))
)
validate_columns(pet.dt, KEEP_COLS, "PET extract")
validate_not_empty(pet.dt, "PET extract")

log_section("Part 2: First scans")
pet.dt <- pet.dt[get(PET$session_col) == 1L]
valid_date.v <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", pet.dt[[PET$date_col]])
log_info("First scans without a valid date, dropped: %d", sum(!valid_date.v))
pet.dt <- pet.dt[valid_date.v]
validate_unique_ids(pet.dt, PET$id_col)

pet.dt <- pet.dt[, .(
  id = get(PET$id_col),
  pet_date = as.IDate(get(PET$date_col)),
  centiloid = get(PET$amyloid_col),
  tau_suvr = get(PET$tau_col),
  amyloid_positive = get(PET$amyloid_status_col) == 1L,
  tau_positive = get(PET$tau_status_col) == 1L
)]
setnames(pet.dt, "id", PET$id_col)
stopifnot(!anyNA(pet.dt[, .(pet_date, centiloid, amyloid_positive)]))
stopifnot(identical(is.na(pet.dt$tau_suvr), is.na(pet.dt$tau_positive)))
log_info("First scans without tau, kept: %d", sum(is.na(pet.dt$tau_suvr)))
# SUVR is a ratio of uptakes, so it is positive
validate_num_range(pet.dt, "tau_suvr", 0, Inf, na_allowed = TRUE)
stopifnot(
  identical(pet.dt$amyloid_positive, pet.dt$centiloid > PET$amyloid_cutoff),
  identical(pet.dt$tau_positive, pet.dt$tau_suvr > PET$tau_cutoff)
)
log_info(
  "%d participants: %d amyloid-positive, %d tau-positive",
  nrow(pet.dt), sum(pet.dt$amyloid_positive),
  sum(pet.dt$tau_positive, na.rm = TRUE)
)

log_section("Part 3: Save results")
results.lst <- list(
  data = pet.dt,
  n = list(
    participants = nrow(pet.dt),
    amyloid_positive = sum(pet.dt$amyloid_positive),
    tau_positive = sum(pet.dt$tau_positive, na.rm = TRUE),
    tau_missing = sum(is.na(pet.dt$tau_suvr)),
    dropped_undated = sum(!valid_date.v)
  ),
  config = list(source = basename(raw.path), pet = PET),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "first PET scans")
log_script_end("04_extract_pet.R", success = TRUE)
