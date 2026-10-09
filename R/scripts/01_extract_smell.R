#!/usr/bin/env Rscript
# =============================================================================
# 01_extract_smell.R - Extract the UPSIT assessments used for modelling
# =============================================================================
# Keeps the participant ID, assessment date, age, UPSIT total score,
# olfactory diagnosis and visit label from the smell export, and adds sex and
# education from the demographics export and APOE e4 carrier status from the
# genotype. Visits without a score, or with the score that stands for
# missing (smell.missing_score), are dropped and counted. Seven follow-up
# visits have wrong dates in the export; they are replaced with those
# confirmed by the data provider.
#
# INPUTS:
#   - data/raw/: smell, demographics and genetics exports named in the config
#   - data/external/smell_date_corrections.csv: confirmed visit dates
# OUTPUTS:
#   - data/derivatives/smell.rds: one row per UPSIT assessment
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))

log_script_start("01_extract_smell.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "extract_smell", default = FALSE
)
output.path <- get_data_path("derivatives", "smell")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("01_extract_smell.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
KEEP_COLS <- c(
  SMELL$id_col, SMELL$date_col, SMELL$raw_age_col, SMELL$score_col,
  SMELL$diagnosis_col, SMELL$visit_col, SMELL$other_cols
)
DEMO_COLS <- c(SMELL$id_col, SMELL$sex_col, SMELL$education_col)
GENE_COLS <- c(SMELL$id_col, SMELL$apoe_genotype_col)

log_section("Part 1: Load data")
raw.path <- get_data_path("raw", "smell_csv")
smell.dt <- read_csv_safe(
  raw.path,
  select = KEEP_COLS,
  # Read as text so IDs with leading zeros keep them
  colClasses = list(character = SMELL$id_col)
)
validate_columns(smell.dt, KEEP_COLS, "smell export")
validate_not_empty(smell.dt, "smell export")

unscored.v <- is.na(smell.dt[[SMELL$score_col]])
log_info("Visits without a score, dropped: %d", sum(unscored.v))
smell.dt <- smell.dt[!unscored.v]
missing_score.v <- smell.dt[[SMELL$score_col]] == SMELL$missing_score
log_info("Visits with a missing score (%g), dropped: %d",
         SMELL$missing_score, sum(missing_score.v))
smell.dt <- smell.dt[!missing_score.v]
diagnosis.v <- smell.dt[[SMELL$diagnosis_col]]
stopifnot(!anyNA(diagnosis.v), all(nzchar(diagnosis.v)))
validate_num_range(
  smell.dt, SMELL$score_col,
  as.numeric(SMELL$score_range[1]), as.numeric(SMELL$score_range[2]),
  na_allowed = FALSE
)
# Candidate_Age is in months: divided by 12, its change between visits
# matches days / 365.25 to within 0.012 years
smell.dt[, (SMELL$age_col) := get(SMELL$raw_age_col) / 12]
smell.dt[, (SMELL$raw_age_col) := NULL]

log_section("Part 2: Date corrections")
fix.dt <- read_csv_safe(
  get_data_path("external", "smell_date_corrections"),
  colClasses = list(character = SMELL$id_col)
)
fix_rows.v <- smell.dt[
  fix.dt, on = c(SMELL$id_col, SMELL$visit_col), which = TRUE
]
stopifnot(length(fix_rows.v) == nrow(fix.dt), !anyNA(fix_rows.v))
# The ages on these rows were computed from the wrong date, so they move by
# the same interval; on all other rows, age changes match days / 365.25 to
# within 0.012 years
shift_years.v <- as.numeric(
  fix.dt[[SMELL$date_col]] - smell.dt[[SMELL$date_col]][fix_rows.v]
) / 365.25
set(smell.dt, fix_rows.v, SMELL$date_col, fix.dt[[SMELL$date_col]])
set(
  smell.dt, fix_rows.v, SMELL$age_col,
  smell.dt[[SMELL$age_col]][fix_rows.v] + shift_years.v
)
smell.dt[, (SMELL$visit_col) := NULL]
# gbmt stops on repeated time points within a participant
validate_unique_ids(smell.dt, c(SMELL$id_col, SMELL$date_col))

log_section("Part 3: Covariates")
demo.dt <- read_csv_safe(
  get_data_path("raw", "demographics_csv"),
  select = DEMO_COLS, colClasses = list(character = SMELL$id_col)
)
gene.dt <- read_csv_safe(
  get_data_path("raw", "genetics_csv"),
  select = GENE_COLS, colClasses = list(character = SMELL$id_col)
)
validate_columns(demo.dt, DEMO_COLS, "demographics export")
validate_columns(gene.dt, GENE_COLS, "genetics export")
validate_unique_ids(demo.dt, SMELL$id_col)
validate_unique_ids(gene.dt, SMELL$id_col)
stopifnot(
  all(smell.dt[[SMELL$id_col]] %in% demo.dt[[SMELL$id_col]]),
  all(smell.dt[[SMELL$id_col]] %in% gene.dt[[SMELL$id_col]])
)

genotype.v <- gene.dt[[SMELL$apoe_genotype_col]]
genotype.v[genotype.v == ""] <- NA
stopifnot(all(grepl("^[234] [234]$", genotype.v[!is.na(genotype.v)])))
gene.dt[, (SMELL$apoe_col) := as.integer(grepl("4", genotype.v))]
gene.dt[is.na(genotype.v), (SMELL$apoe_col) := NA]
gene.dt[, (SMELL$apoe_genotype_col) := NULL]

smell.dt <- demo.dt[smell.dt, on = SMELL$id_col]
smell.dt <- gene.dt[smell.dt, on = SMELL$id_col]
log_info(
  "Participants with sex, education or APOE missing: %d, %d, %d",
  uniqueN(smell.dt[is.na(get(SMELL$sex_col)), get(SMELL$id_col)]),
  uniqueN(smell.dt[is.na(get(SMELL$education_col)), get(SMELL$id_col)]),
  uniqueN(smell.dt[is.na(get(SMELL$apoe_col)), get(SMELL$id_col)])
)
setorderv(smell.dt, c(SMELL$id_col, SMELL$date_col))

log_section("Part 4: Save results")
results.lst <- list(
  data = smell.dt,
  n = list(
    assessments = nrow(smell.dt),
    participants = uniqueN(smell.dt[[SMELL$id_col]]),
    corrected_dates = nrow(fix.dt),
    dropped_unscored = sum(unscored.v),
    dropped_missing_score = sum(missing_score.v)
  ),
  config = list(source = basename(raw.path), smell = SMELL),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "extracted smell data")
log_script_end("01_extract_smell.R", success = TRUE)
