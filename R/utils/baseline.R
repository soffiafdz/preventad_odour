# =============================================================================
# baseline.R - First UPSIT assessment matched to the RBANS of the same visit
# =============================================================================
# Project-specific. Shared by the analyses of RBANS by baseline UPSIT, so
# they all use the same participants and the same baseline.
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/validation.R"))

#' One row per participant: first UPSIT and the RBANS of the same visit
#'
#' The first assessment follows the visit exclusions of the trajectory
#' models. Participants without an RBANS at that visit (Study_visit_label)
#' are left out and counted in the log. UPSIT is divided by
#' rbans_baseline.upsit_step and age at the first assessment is
#' (age - trajectories.age_centre) / trajectories.time_divisor.
#'
#' @param smell.dt Data from 01_extract_smell.R
#' @param rbans.dt Data from 09_extract_rbans.R
#' @return data.table with id, visit, upsit_date, rbans_date, upsit,
#'   covariates and the RBANS scores at baseline; attribute n_first is the
#'   number of participants with a first assessment
build_baseline <- function(smell.dt, rbans.dt) {
  smell <- get_parameter("smell")
  rbans <- get_parameter("rbans")
  traj <- get_parameter("trajectories")
  step <- get_parameter("rbans_baseline")$upsit_step
  score_cols.v <- unlist(rbans$score_cols)

  first.dt <- smell.dt[!get(traj$exclude_col) %in% traj$exclude_values]
  setorderv(first.dt, c(smell$id_col, smell$date_col))
  first.dt <- first.dt[, .SD[1], by = c(smell$id_col)]
  first.dt <- first.dt[, .(
    id = get(smell$id_col),
    visit = get(smell$other_cols[[1]]),
    upsit_date = get(smell$date_col),
    upsit = get(smell$score_col) / step,
    age_first = (get(smell$age_col) - traj$age_centre) / traj$time_divisor,
    female = as.numeric(get(smell$sex_col) == smell$female_value),
    education = as.numeric(get(smell$education_col)),
    apoe_carrier = as.numeric(
      get(smell$apoe_col) == smell$apoe_carrier_value
    )
  )]

  visit.dt <- rbans.dt[, c(
    list(
      id = get(rbans$id_col), visit = get(rbans$visit_col),
      rbans_date = get(rbans$date_col)
    ),
    .SD
  ), .SDcols = score_cols.v]
  baseline.dt <- visit.dt[first.dt, on = c("id", "visit"), nomatch = NULL]
  validate_unique_ids(baseline.dt, "id")
  validate_not_empty(baseline.dt, "first assessments with RBANS")
  gap_days.v <- abs(
    as.numeric(baseline.dt$rbans_date - baseline.dt$upsit_date)
  )
  log_info(
    paste(
      "%d of %d participants have an RBANS at their first UPSIT visit;",
      "days apart: median %g, max %g"
    ),
    nrow(baseline.dt), nrow(first.dt), median(gap_days.v), max(gap_days.v)
  )
  setattr(baseline.dt, "n_first", nrow(first.dt))
  baseline.dt[]
}
