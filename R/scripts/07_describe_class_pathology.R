#!/usr/bin/env Rscript
# =============================================================================
# 07_describe_class_pathology.R - PET pathology in each trajectory class
# =============================================================================
# Descriptive only: for the configured time scale's trajectory classes,
# counts and percentages of amyloid- and tau-positive participants, A/T
# combinations, and median Centiloid and tau meta-ROI SUVR at the first PET.
# The classes other than the main one are too small for a formal model of
# class membership, so no tests are reported; for one configured class, the
# difference from the main (largest) class in the share of amyloid- and
# tau-positive participants is given with an approximate 95% CI.
# Percentages are of the participants with PET.
#
# INPUTS:
#   - data/derivatives/trajectory_classes*.rds: from 03_describe_trajectories.R
#   - data/derivatives/pet.rds: from 04_extract_pet.R
# OUTPUTS:
#   - outputs/tables/trajectory_classes_pathology*: table, HTML
#   - models/results/class_pathology*.rds: the numbers behind it
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))
source(here("R/utils/tables.R"))

log_script_start("07_describe_class_pathology.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "describe_class_pathology", default = FALSE
)
SCALES <- get_parameter("trajectories", "time_scales")
output_paths.v <- vapply(
  SCALES, \(s) get_data_path("models", "class_pathology", s), character(1)
)
pending.v <- SCALES[FORCE_REGENERATE | !file.exists(output_paths.v)]
if (length(pending.v) == 0) {
  log_info("Outputs exist and force_regenerate=FALSE; skipping")
  log_script_end("07_describe_class_pathology.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
PET <- get_parameter("pet")
TRAJ <- get_parameter("trajectories")
ID <- get_parameter("smell", "id_col")

for (SCALE in pending.v) {
  log_section(sprintf("Time scale: %s", SCALE))
  if (identical(TRAJ$selected_model[[SCALE]], "pending")) {
    log_warn("%s: model selection pending; skipped", SCALE)
    next
  }
  LABELS <- require_output(TRAJ$class_labels[[SCALE]], "class labels")
  COMPARED <- require_output(
    TRAJ$pathology_comparison_class[[SCALE]], "compared class"
  )
  stopifnot(COMPARED %in% LABELS)
  log_section("Part 1: Load data")
  classes.dt <- read_rds_safe(
    get_data_path("derivatives", "trajectory_classes", SCALE)
  )
  pet.dt <- read_rds_safe(get_data_path("derivatives", "pet"))$data
  merged.dt <- pet.dt[classes.dt, on = ID]
  validate_not_empty(merged.dt, "classes with PET")

  log_section("Part 2: Summaries by class")
  #' n (%) with a denominator
  #' @param count,n Integers
  #' @return Character string
  n_pct.fn <- function(count, n) sprintf("%d (%.0f%%)", count, 100 * count / n)
  #' Median (first quartile to third quartile)
  #' @param x Numeric vector
  #' @param digits Decimal places
  #' @return Character string
  median_iqr.fn <- function(x, digits) {
    q.v <- quantile(x, c(0.5, 0.25, 0.75), na.rm = TRUE, names = FALSE)
    sprintf("%.*f (%.*f to %.*f)", digits, q.v[1], digits, q.v[2], digits,
            q.v[3])
  }
  #' Pathology summary of one group of participants
  #' @param group.dt Rows of merged.dt
  #' @param group Group label
  #' @return One-row data.table of counts and summaries
  summarise_group.fn <- function(group.dt, group) {
    pet_rows.dt <- group.dt[!is.na(centiloid)]
    n_pet <- nrow(pet_rows.dt)
    at_count.fn <- function(a, t) {
      pet_rows.dt[amyloid_positive == a & tau_positive == t, .N]
    }
    data.table(
      group = group,
      n = nrow(group.dt),
      n_pet = n_pet,
      n_amyloid_positive = sum(pet_rows.dt$amyloid_positive),
      n_tau = sum(!is.na(pet_rows.dt$tau_positive)),
      n_tau_positive = sum(pet_rows.dt$tau_positive, na.rm = TRUE),
      n_a_neg_t_neg = at_count.fn(FALSE, FALSE),
      n_a_neg_t_pos = at_count.fn(FALSE, TRUE),
      n_a_pos_t_neg = at_count.fn(TRUE, FALSE),
      n_a_pos_t_pos = at_count.fn(TRUE, TRUE),
      centiloid = median_iqr.fn(pet_rows.dt$centiloid, 1),
      tau_suvr = median_iqr.fn(pet_rows.dt$tau_suvr, 2)
    )
  }
  summary.dt <- rbind(
    rbindlist(lapply(LABELS, \(lab) {
      summarise_group.fn(merged.dt[class == lab], lab)
    })),
    summarise_group.fn(merged.dt, "All")
  )
  main.dt <- summary.dt[group != "All"][which.max(n)]
  compared.dt <- summary.dt[group == COMPARED]

  #' Difference in proportions with an approximate (Wald) 95% CI
  #' @param x1,n1 Count and denominator of the compared class
  #' @param x0,n0 Count and denominator of the main class
  #' @return Character string in percentage points
  difference_ci.fn <- function(x1, n1, x0, n0) {
    p1 <- x1 / n1
    p0 <- x0 / n0
    d <- p1 - p0
    se <- sqrt(p1 * (1 - p1) / n1 + p0 * (1 - p0) / n0)
    sprintf("%+.0f (%.0f to %.0f)", 100 * d, 100 * (d - 1.96 * se),
            100 * (d + 1.96 * se))
  }
  difference_label <- sprintf(
    "difference from %s, percentage points (95%% CI)", main.dt$group
  )

  table.dt <- data.table(characteristic = c(
    "Participants, n",
    "With PET, n (% of participants)",
    sprintf("Amyloid positive (Centiloid > %g), n (%%)", PET$amyloid_cutoff),
    paste("Amyloid positive,", difference_label),
    sprintf(
      "Tau positive (meta-ROI > %g), n (%% of those with tau)", PET$tau_cutoff
    ),
    paste("Tau positive,", difference_label),
    "A-T- / A-T+ / A+T- / A+T+, n",
    "Centiloid, median (IQR)",
    "Tau meta-ROI SUVR, median (IQR)"
  ))
  #' Difference from the main class, shown only in the compared class
  #' @param grp Column being filled
  #' @param col Count column of summary.dt
  #' @param denominator Column of summary.dt with the matching denominator
  #' @return Character string
  difference_cell.fn <- function(grp, col, denominator) {
    if (grp != COMPARED) return("\u2013")
    difference_ci.fn(
      compared.dt[[col]], compared.dt[[denominator]],
      main.dt[[col]], main.dt[[denominator]]
    )
  }
  for (grp in summary.dt$group) {
    s <- summary.dt[group == grp]
    set(table.dt, j = grp, value = c(
      as.character(s$n),
      n_pct.fn(s$n_pet, s$n),
      n_pct.fn(s$n_amyloid_positive, s$n_pet),
      difference_cell.fn(grp, "n_amyloid_positive", "n_pet"),
      n_pct.fn(s$n_tau_positive, s$n_tau),
      difference_cell.fn(grp, "n_tau_positive", "n_tau"),
      sprintf(
        "%d / %d / %d / %d",
        s$n_a_neg_t_neg, s$n_a_neg_t_pos, s$n_a_pos_t_neg, s$n_a_pos_t_pos
      ),
      s$centiloid,
      s$tau_suvr
    ))
  }

  log_section("Part 3: Table")
  table_path <- get_data_path("outputs", "class_pathology_table", SCALE)
  class_pathology.tbl <- create_class_table.fn(
    table.dt,
    title = "**Amyloid and tau PET in the UPSIT trajectory classes.**",
    source_note = sprintf(
      paste(
        "Classes from the selected trajectory model on %s. First PET scan;",
        "percentages of amyloid and tau positivity are of participants with",
        "PET. Differences between the %s and %s classes are in percentage",
        "points with approximate (Wald) 95%% confidence intervals. No",
        "p-values are shown: with %d participants with PET in the %s class,",
        "a test would have little chance of detecting a difference of this",
        "size, so a non-significant result would not mean there is no",
        "association. The other classes are too small to compare."
      ),
      if (SCALE == "age") "age" else "years since the first assessment",
      tolower(COMPARED), tolower(main.dt$group), compared.dt$n_pet,
      tolower(COMPARED)
    )
  )
  save_table(
    class_pathology.tbl, basename(table_path), dirname(table_path),
    formats = get_config("output", "table_formats")
  )

  log_section("Part 4: Save results")
  results.lst <- list(
    summary = summary.dt,
    table = table.dt,
    outputs = list(table = table_path),
    n = list(participants = nrow(merged.dt),
             with_pet = sum(!is.na(merged.dt$centiloid))),
    config = list(pet = PET, time_scale = SCALE),
    timestamp = Sys.time()
  )
  write_rds_safe(
    results.lst, output_paths.v[[SCALE]], "class pathology description"
  )
}
log_script_end("07_describe_class_pathology.R", success = TRUE)
