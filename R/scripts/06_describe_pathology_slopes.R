#!/usr/bin/env Rscript
# =============================================================================
# 06_describe_pathology_slopes.R - Figure and table of UPSIT change by PET
# =============================================================================
# From the main pathology models in 05: predicted UPSIT over years since the
# first assessment (lcmm::predictY, 95% CIs) for amyloid-negative and
# -positive, tau-negative and -positive, and the A/T groups, for age 65 at
# the first assessment and average covariates. Each curve is drawn over the
# follow-up its group covers. The table gives change per year by group, the
# differences, the continuous effects, and the differences after also
# adjusting for APOE e4 and education.
#
# INPUTS:
#   - models/results/pathology_slopes.rds: from 05_fit_pathology_slopes.R
# OUTPUTS:
#   - outputs/figures/upsit_pathology_slopes: figure, PDF and PNG
#   - outputs/figures/upsit_pathology_slopes_slide: figure for slides, PNG
#   - outputs/tables/upsit_pathology_slopes: table, HTML
#   - models/results/pathology_description.rds: the numbers behind them
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
source(here("R/utils/plotting.R"))
source(here("R/utils/tables.R"))

log_script_start("06_describe_pathology_slopes.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "describe_pathology", default = FALSE
)
output.path <- get_data_path("models", "pathology_description")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("06_describe_pathology_slopes.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
PET <- get_parameter("pet")
PATH <- get_parameter("pathology")
TRAJ <- get_parameter("trajectories")
FIGURE_FORMATS <- get_config("output", "figure_format")
DPI <- get_config("output", "dpi")
FIGURE_SIZE <- get_config("output", "figure_size")
BASE_SIZE <- get_config("output", "base_size")
SLIDE_SIZE <- get_config("output", "slide_figure_size")
SLIDE_BASE_SIZE <- get_config("output", "slide_base_size")

log_section("Part 1: Load results")
slopes.res <- read_rds_safe(get_data_path("models", "pathology_slopes"))
participants.dt <- slopes.res$participants
coefficients.dt <- slopes.res$coefficients
validate_not_empty(participants.dt, "participants")

# Groups shown, with the fit, the predictor values and the members of each
at_colour.v <- c(
  `A-T-` = "pathology_negative", `A+T-` = "pathology_amyloid_only",
  `A+T+` = "pathology_positive"
)
stopifnot(all(PATH$at_groups %in% names(at_colour.v)))
n_at <- length(PATH$at_groups)
group.dt <- data.table(
  panel = c(rep("Amyloid", 2), rep("Tau", 2), rep("A/T", n_at)),
  group = c("A-", "A+", "T-", "T+", PATH$at_groups),
  fit = c(
    rep("amyloid_positive_main", 2), rep("tau_positive_main", 2),
    rep("at_groups_main", n_at)
  ),
  colour = c(
    "pathology_negative", "pathology_positive",
    "pathology_negative", "pathology_positive",
    at_colour.v[PATH$at_groups]
  )
)
# %in% keeps the participant without tau out of both tau groups
members.lst <- c(
  list(
    `A-` = !participants.dt$amyloid_positive,
    `A+` = participants.dt$amyloid_positive,
    `T-` = participants.dt$tau_positive %in% FALSE,
    `T+` = participants.dt$tau_positive %in% TRUE
  ),
  setNames(
    lapply(PATH$at_groups, \(g) participants.dt$at_group == g),
    PATH$at_groups
  )
)
group.dt[, `:=`(
  n = vapply(group, \(g) sum(members.lst[[g]]), integer(1)),
  max_years = vapply(
    group, \(g) max(participants.dt$follow_up[members.lst[[g]]]), numeric(1)
  )
)]

log_section("Part 2: Predicted trajectories")
at_dummies.v <- slopes.res$at_dummies
#' Predictor values identifying a group in its model
#' @param group Group label
#' @return Named list of predictor values
group_values.fn <- function(group) {
  switch(group,
    `A-` = list(amyloid_positive = 0),
    `A+` = list(amyloid_positive = 1),
    `T-` = list(tau_positive = 0),
    `T+` = list(tau_positive = 1),
    # A/T groups: each indicator is 1 for its own group, the reference has none
    setNames(
      as.list(as.numeric(names(at_dummies.v) == group)), at_dummies.v
    )
  )
}
curves.dt <- rbindlist(lapply(seq_len(nrow(group.dt)), \(i) {
  g <- group.dt[i]
  years.v <- seq(0, g$max_years, by = 0.1)
  # Covariates at 0: age 65 at the first assessment, average sex
  newdata.df <- data.frame(
    years = years.v, age_first = 0, female = 0, group_values.fn(g$group)
  )
  predicted <- predictY(
    slopes.res$fits[[g$fit]], newdata.df, var.time = "years", draws = TRUE
  )$pred
  data.table(
    panel = g$panel, group = g$group, time = years.v,
    fit = predicted[, "Ypred"], lower = predicted[, "lower.Ypred"],
    upper = predicted[, "upper.Ypred"]
  )
}))
curves.dt[, panel := factor(panel, c("Amyloid", "Tau", "A/T"))]
ends.dt <- curves.dt[, .SD[.N], by = .(panel, group)]
# Labels with a true minus sign (docs/STYLE.md); groups keep their keys
minus.fn <- function(x) gsub("-", "\u2212", x, fixed = TRUE)
ends.dt[group.dt, on = "group",
        label := sprintf("%s (n = %d)", minus.fn(group), i.n)]

log_section("Part 3: Figure")
colors.v <- setNames(
  unlist(manuscript_colors()[group.dt$colour]), group.dt$group
)
amyloid_diff.dt <- coefficients.dt[
  model == "amyloid_positive_main" &
    quantity == "Difference in slope (points per year)"
]
not_shown.dt <- participants.dt[
  !at_group %in% PATH$at_groups, .N, by = at_group
]
not_shown_text <- paste(
  sprintf("%s (n = %d)", not_shown.dt$at_group, not_shown.dt$N),
  collapse = " and "
)
n_tau_positive <- sum(participants.dt$tau_positive, na.rm = TRUE)
pathology.plt <- plot_pathology_trajectories.fn(
  curves.dt, ends.dt, colors.v,
  x_lab = "Years since first UPSIT",
  title = "**UPSIT change over time by amyloid and tau status.**",
  subtitle = sprintf(
    "PREVENT-AD, %d participants with PET, %d UPSIT assessments",
    slopes.res$n$participants, slopes.res$n$assessments
  ),
  caption = sprintf(
    paste(
      "Predicted UPSIT with 95%% confidence bands from linear mixed models",
      "(lcmm), for age %g at the first assessment and average sex; each",
      "line covers its group's follow-up. Positivity at the first PET:",
      "Centiloid > %g, tau meta-ROI > %g. PET was taken a median of %.1f",
      "years after the first UPSIT, so these are associations with later",
      "pathology. Amyloid-positive participants lost %.2f more points per",
      "year (95%% CI %.2f to %.2f). Only %d participants are tau-positive,",
      "so tau comparisons are imprecise. Not shown: %s."
    ),
    TRAJ$age_centre, PET$amyloid_cutoff, PET$tau_cutoff,
    median(participants.dt$years_to_pet), -amyloid_diff.dt$estimate,
    -amyloid_diff.dt$upper, -amyloid_diff.dt$lower, n_tau_positive,
    not_shown_text
  ),
  base_size = BASE_SIZE
)
# The slide heading and notes carry the title and caption
pathology_slide.plt <- plot_pathology_trajectories.fn(
  curves.dt, ends.dt, colors.v,
  x_lab = "Years since first UPSIT",
  title = NULL, subtitle = NULL, caption = NULL, base_size = SLIDE_BASE_SIZE
)
figure_paths.v <- vapply(FIGURE_FORMATS, \(fmt) {
  save_plot(
    pathology.plt,
    paste0(get_data_path("outputs", "pathology_figure"), ".", fmt),
    width = FIGURE_SIZE[1], height = FIGURE_SIZE[2], dpi = DPI
  )
}, character(1))
figure_paths.v <- c(
  figure_paths.v,
  slide = save_plot(
    pathology_slide.plt,
    paste0(get_data_path("outputs", "pathology_slide_figure"), ".png"),
    width = SLIDE_SIZE[1], height = SLIDE_SIZE[2], dpi = DPI
  )
)

log_section("Part 4: Table")
#' Estimate with 95% CI, or a dash when the row has none
#' @param x.dt Rows of coefficients.dt (zero or one)
#' @return Character string
estimate_ci.fn <- function(x.dt) {
  if (nrow(x.dt) == 0) return("–")
  format_effect_ci(x.dt$estimate, x.dt$lower, x.dt$upper, digits = 2)
}
#' One table row from a model's coefficients
#' @param section,row Labels
#' @param marker Marker model name prefix
#' @param slope_quantity Quantity giving the row's own slope, or NA
#' @param difference Whether the row shows the marker's slope difference
#' @return One-row data.table
table_row.fn <- function(section, row, marker, slope_quantity, difference) {
  # Names are built outside [ ]: marker, adjustment and quantity are also
  # columns of coefficients.dt and would take precedence inside it
  pick.fn <- function(adjustment_name, quantity_name) {
    model_name <- paste(marker, adjustment_name, sep = "_")
    coefficients.dt[model == model_name & quantity == quantity_name]
  }
  diff_q <- "Difference in slope (points per year)"
  main_diff.dt <- if (difference) pick.fn("main", diff_q) else NULL
  data.table(
    section = section,
    row = row,
    slope = if (is.na(slope_quantity)) {
      "–"
    } else {
      estimate_ci.fn(pick.fn("main", slope_quantity))
    },
    difference = if (difference) estimate_ci.fn(main_diff.dt) else "–",
    p = if (difference) format_p(main_diff.dt$p) else "–",
    difference_sensitivity = if (difference) {
      estimate_ci.fn(pick.fn("sensitivity", diff_q))
    } else {
      "–"
    }
  )
}
n_of.fn <- function(g) group.dt[group == g, n]
ref_q <- "Reference slope (points per year)"
pos_q <- "Slope if positive (points per year)"
amyloid_section <- sprintf("Amyloid (Centiloid > %g)", PET$amyloid_cutoff)
tau_section <- sprintf("Tau (meta-ROI SUVR > %g)", PET$tau_cutoff)
continuous_section <- "Continuous, per step above the cutoff"
slopes.dt <- rbind(
  table_row.fn(amyloid_section, sprintf("Negative (n = %d)", n_of.fn("A-")),
               "amyloid_positive", ref_q, FALSE),
  table_row.fn(amyloid_section, sprintf("Positive (n = %d)", n_of.fn("A+")),
               "amyloid_positive", pos_q, TRUE),
  table_row.fn(tau_section, sprintf("Negative (n = %d)", n_of.fn("T-")),
               "tau_positive", ref_q, FALSE),
  table_row.fn(tau_section, sprintf("Positive (n = %d)", n_of.fn("T+")),
               "tau_positive", pos_q, TRUE),
  table_row.fn(continuous_section,
               sprintf("Per %g Centiloids", PATH$centiloid_step),
               "centiloid", NA, TRUE),
  table_row.fn(continuous_section,
               sprintf("Per %g SUVR of tau meta-ROI", PATH$tau_step),
               "tau_suvr", NA, TRUE)
)
at.dt <- slopes.res$group_slopes
slopes.dt <- rbind(slopes.dt, data.table(
  section = "A/T groups",
  row = sprintf("%s (n = %d)", minus.fn(at.dt$group), at.dt$participants),
  slope = format_effect_ci(at.dt$estimate, at.dt$lower, at.dt$upper, 2),
  difference = "–", p = "–", difference_sensitivity = "–"
))

table_path <- get_data_path("outputs", "pathology_table")
pathology.tbl <- create_pathology_table.fn(
  slopes.dt,
  column_labels.lst = list(
    slope = "Change per year [95% CI]",
    difference = "Difference per year [95% CI]",
    p = "p",
    difference_sensitivity = "Difference, also adjusted [95% CI]"
  ),
  title = "**UPSIT change per year by amyloid and tau.**",
  source_note = sprintf(
    paste(
      "UPSIT points per year from linear mixed models over years since the",
      "first assessment, adjusted for age at the first assessment and sex,",
      "for age %g and average sex. Differences are against the negative",
      "group, or per step above the positivity cutoff. Also adjusted: plus",
      "APOE ε4 carrier status and education. Only %d participants are",
      "tau-positive, so tau comparisons are imprecise. Not modelled: %s."
    ),
    TRAJ$age_centre, n_tau_positive, not_shown_text
  )
)
save_table(
  pathology.tbl, basename(table_path), dirname(table_path),
  formats = get_config("output", "table_formats")
)

log_section("Part 5: Save results")
results.lst <- list(
  table = slopes.dt,
  curves = curves.dt,
  groups = group.dt,
  outputs = list(figures = unname(figure_paths.v), table = table_path),
  n = slopes.res$n,
  config = list(pet = PET, pathology = PATH),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "pathology description")
log_script_end("06_describe_pathology_slopes.R", success = TRUE)
