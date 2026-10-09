#!/usr/bin/env Rscript
# =============================================================================
# 14_describe_rbans_mci.R - Figures and table of memory and MCI by UPSIT
# =============================================================================
# From the main models of 12 and 13, for participants at the 25th and 75th
# percentiles of baseline UPSIT: predicted delayed memory over years since
# the first UPSIT (fixed effects with 95% CIs; age 65 at the first UPSIT,
# average sex and education, test version A, practice gain left out), and
# model-based cumulative incidence of MCI (95% CIs; average age, sex and
# education). The table gives H1 to H4 for every RBANS score and for MCI.
# From 11: each person's estimated yearly change in UPSIT and in delayed
# memory (fixed slope plus their random slope), with the model's correlation
# of the random slopes.
#
# INPUTS:
#   - models/results/rbans_baseline.rds: from 10_fit_rbans_baseline.R
#   - models/results/rbans_change.rds: from 12_fit_rbans_change.R
#   - models/results/mci_progression.rds: from 13_fit_mci_progression.R
#   - models/results/slope_coupling.rds: from 11_fit_slope_coupling.R
# OUTPUTS:
#   - outputs/figures/delayed_memory_by_upsit: figure, PDF and PNG, and a
#     slide version (PNG)
#   - outputs/figures/mci_by_upsit: figure, PDF and PNG, and a slide version
#   - outputs/figures/upsit_memory_slopes: figure, PDF and PNG, and a slide
#     version (PNG)
#   - outputs/tables/rbans_mci_by_upsit: table, HTML
#   - models/results/rbans_mci_description.rds: the numbers behind them
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
  library(nlme)
  library(survival)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))
source(here("R/utils/plotting.R"))
source(here("R/utils/tables.R"))

log_script_start("14_describe_rbans_mci.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "describe_rbans_mci", default = FALSE
)
output.path <- get_data_path("models", "rbans_mci_description")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("14_describe_rbans_mci.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
RBANS <- get_parameter("rbans")
TRAJ <- get_parameter("trajectories")
STEP <- get_parameter("rbans_baseline")$upsit_step
VERSIONS <- unlist(RBANS$versions)
PRIMARY <- "delayed_memory_index_score_60_69"
SCORE_LABELS <- c(
  total_scale_index_score_60_69 = "Total scale",
  immediate_memory_index_score_60_69 = "Immediate memory",
  delayed_memory_index_score_60_69 = "Delayed memory"
)
stopifnot(setequal(names(SCORE_LABELS), unlist(RBANS$score_cols)))
PERCENTILES <- c(low = 0.25, high = 0.75)
FIGURE_FORMATS <- get_config("output", "figure_format")
DPI <- get_config("output", "dpi")
FIGURE_SIZE <- get_config("output", "figure_size")
BASE_SIZE <- get_config("output", "base_size")
SLIDE_SIZE <- get_config("output", "slide_figure_size")
SLIDE_BASE_SIZE <- get_config("output", "slide_base_size")

log_section("Part 1: Load results")
baseline.res <- read_rds_safe(get_data_path("models", "rbans_baseline"))
change.res <- read_rds_safe(get_data_path("models", "rbans_change"))
mci.res <- read_rds_safe(get_data_path("models", "mci_progression"))
coupling.res <- read_rds_safe(get_data_path("models", "slope_coupling"))
colors.v <- unlist(manuscript_colors()[c("upsit_low", "upsit_high")])

#' Number with a true minus sign (docs/STYLE.md)
#' @param x Number
#' @param digits Decimals
#' @return Character string
signed.fn <- function(x, digits = 2) {
  sub("^-", "\u2212", formatC(x, format = "f", digits = digits))
}

#' Group labels for the two UPSIT levels
#' @param points.v UPSIT at the two percentiles, named low and high
#' @return Character vector named low and high
level_labels.fn <- function(points.v) {
  c(
    low = sprintf("UPSIT %g (25th percentile)", points.v[["low"]]),
    high = sprintf("UPSIT %g (75th percentile)", points.v[["high"]])
  )
}

#' Curves as steps, so that lines and bands change only at event times
#' @param curve.dt One group's curve, ordered by time
#' @return data.table with each value repeated at the next time
as_steps.fn <- function(curve.dt) {
  n <- nrow(curve.dt)
  # At each time the previous value comes first, then the new one
  steps.dt <- rbind(
    curve.dt[, step := 2L],
    curve.dt[-n][, `:=`(time = curve.dt$time[-1], step = 1L)]
  )
  steps.dt[order(time, step)][, step := NULL][]
}

log_section("Part 2: Delayed memory")
memory.fit <- change.res$fits[[paste0(PRIMARY, "_main")]]
person.dt <- unique(as.data.table(memory.fit$data)[, .(subject, upsit)])
# UPSIT in the model is in steps, centred at the sample mean
memory_points.v <- quantile(
  (person.dt$upsit + change.res$upsit_mean) * STEP, PERCENTILES, type = 1,
  names = FALSE
)
names(memory_points.v) <- names(PERCENTILES)
memory_years <- quantile(
  as.data.table(memory.fit$data)[, max(years), by = subject]$V1, 0.9,
  names = FALSE
)
memory_labels.v <- level_labels.fn(memory_points.v)
memory.dt <- rbindlist(lapply(names(PERCENTILES), \(level) {
  newdata.df <- data.frame(
    years = seq(0, memory_years, by = 0.1),
    upsit = memory_points.v[[level]] / STEP - change.res$upsit_mean,
    age_first = 0, female = 0, education = 0, retest = 0
  )
  for (v in VERSIONS[-1]) newdata.df[[paste0("version_", v)]] <- 0
  x.mat <- model.matrix(
    delete.response(terms(formula(memory.fit))), newdata.df
  )
  fit.v <- drop(x.mat %*% fixef(memory.fit))
  se.v <- sqrt(rowSums((x.mat %*% vcov(memory.fit)) * x.mat))
  data.table(
    panel = "Delayed memory", group = memory_labels.v[[level]],
    time = newdata.df$years, fit = fit.v,
    lower = fit.v - 1.96 * se.v, upper = fit.v + 1.96 * se.v
  )
}))
memory_ends.dt <- memory.dt[, .SD[.N], by = group][, label := group]

coefficients.dt <- change.res$coefficients
pick_change.fn <- function(score_col, adjustment_name) {
  model_name <- paste(score_col, adjustment_name, sep = "_")
  row.dt <- coefficients.dt[
    model == model_name & grepl("^Difference in annual change", quantity)
  ]
  stopifnot(nrow(row.dt) == 1)
  row.dt
}
memory_effect.dt <- pick_change.fn(PRIMARY, "main")
memory_colors.v <- setNames(colors.v, memory_labels.v[c("low", "high")])
memory_caption <- sprintf(
  paste(
    "Predicted RBANS delayed memory index (60-69 norms, not adjusted for",
    "age) with 95%% confidence bands, from a linear mixed model with a",
    "random intercept and slope; for age %g at the first UPSIT, average sex",
    "and education, test version A and without the practice gain at the",
    "second test. Per %g more UPSIT points, delayed memory declined %s",
    "points per year more slowly (95%% CI %s to %s; p = %.2f)."
  ),
  TRAJ$age_centre, STEP, signed.fn(memory_effect.dt$estimate),
  signed.fn(memory_effect.dt$lower), signed.fn(memory_effect.dt$upper),
  memory_effect.dt$p
)

log_section("Part 3: Cumulative incidence of MCI")
mci.fit <- mci.res$fits$main
# Without the Surv response, which data.table cannot convert
frame.dt <- as.data.table(model.frame(mci.fit)[-1])
# UPSIT in the Cox model is in steps, not centred
mci_points.v <- quantile(
  frame.dt$upsit * STEP, PERCENTILES, type = 1, names = FALSE
)
names(mci_points.v) <- names(PERCENTILES)
# Curves stop at the horizon, while about half the sample is still at risk
mci_years <- get_parameter("mci_progression")$horizon_years
mci_labels.v <- level_labels.fn(mci_points.v)
covariates.v <- setdiff(attr(terms(mci.fit), "term.labels"), "upsit")
newdata.df <- data.frame(upsit = mci_points.v / STEP)
for (col in covariates.v) newdata.df[[col]] <- mean(frame.dt[[col]])
survival.fit <- survfit(mci.fit, newdata = newdata.df)
mci.dt <- rbindlist(lapply(seq_along(PERCENTILES), \(i) {
  curve.dt <- data.table(
    time = c(0, survival.fit$time),
    fit = 100 * (1 - c(1, survival.fit$surv[, i])),
    lower = 100 * (1 - c(1, survival.fit$upper[, i])),
    upper = 100 * (1 - c(1, survival.fit$lower[, i]))
  )[time <= mci_years]
  curve.dt <- rbind(curve.dt, curve.dt[.N][, time := mci_years])
  as_steps.fn(curve.dt)[, `:=`(
    panel = "MCI", group = mci_labels.v[[names(PERCENTILES)[i]]]
  )]
}))
mci_ends.dt <- mci.dt[, .SD[.N], by = group][, label := group]
# Absolute risks at the configured horizon, with the number still at risk
HORIZON <- get_parameter("mci_progression")$horizon_years
horizon.fit <- summary(survival.fit, times = HORIZON)
horizon.dt <- data.table(
  level = names(PERCENTILES), upsit = mci_points.v,
  fit = 100 * (1 - horizon.fit$surv[1, ]),
  lower = 100 * (1 - horizon.fit$upper[1, ]),
  upper = 100 * (1 - horizon.fit$lower[1, ])
)
at_risk_horizon <- sum(mci.fit$y[, "time"] >= HORIZON)
events_horizon <- sum(
  mci.fit$y[, "status"] == 1 & mci.fit$y[, "time"] <= HORIZON
)
hazard.dt <- mci.res$hazard_ratios
main_hr.dt <- hazard.dt[adjustment == "main"]
mci_colors.v <- setNames(colors.v, mci_labels.v[c("low", "high")])
mci_caption <- sprintf(
  paste(
    "Cumulative incidence of MCI with 95%% confidence bands from a Cox",
    "model adjusted for age at the first UPSIT, sex and education, for",
    "average covariates. Per %g more UPSIT points, hazard ratio %.2f (95%%",
    "CI %.2f to %.2f; p = %.3f); %d participants, %d with MCI. MCI reviews",
    "in the export end on %s, so follow-up without MCI ends there.",
    "Proportional hazards held for UPSIT (p = %.2f), not for education."
  ),
  STEP, main_hr.dt$hazard_ratio, main_hr.dt$lower, main_hr.dt$upper,
  main_hr.dt$p, main_hr.dt$n, main_hr.dt$events,
  format(mci.res$n$coverage_end, "%B %Y"), main_hr.dt$ph_p_upsit
)

log_section("Part 4: Figures")
#' Save a manuscript figure and its slide version
#' @param curves.dt,ends.dt,colors.v Curves, end labels and colours
#' @param x_lab,y_lab Axis labels
#' @param title,subtitle,caption Manuscript figure text
#' @param output_key,slide_key Keys of data.outputs
#' @param key_corner Left corner that holds the key, "bottom" or "top"
#' @return Paths of the saved files
save_figures.fn <- function(curves.dt, ends.dt, colors.v, x_lab, y_lab,
                            title, subtitle, caption, output_key,
                            slide_key, key_corner) {
  figure.plt <- plot_pathology_trajectories.fn(
    curves.dt, ends.dt, colors.v, x_lab = x_lab, title = title,
    subtitle = subtitle, caption = caption, base_size = BASE_SIZE,
    y_lab = y_lab, key_corner = key_corner
  )
  # The slide heading and the line under the figure carry the text
  slide.plt <- plot_pathology_trajectories.fn(
    curves.dt, ends.dt, colors.v, x_lab = x_lab, title = NULL,
    subtitle = NULL, caption = NULL, base_size = SLIDE_BASE_SIZE,
    y_lab = y_lab, key_corner = key_corner
  )
  paths.v <- vapply(FIGURE_FORMATS, \(fmt) {
    save_plot(
      figure.plt, paste0(get_data_path("outputs", output_key), ".", fmt),
      width = FIGURE_SIZE[1], height = FIGURE_SIZE[2], dpi = DPI
    )
  }, character(1))
  c(paths.v, slide = save_plot(
    slide.plt, paste0(get_data_path("outputs", slide_key), ".png"),
    width = SLIDE_SIZE[1], height = SLIDE_SIZE[2], dpi = DPI
  ))
}
memory_paths.v <- save_figures.fn(
  memory.dt, memory_ends.dt, memory_colors.v,
  x_lab = "Years since first UPSIT",
  y_lab = "Predicted delayed memory index",
  title = "**Delayed memory over time by baseline smell.**",
  subtitle = sprintf(
    "PREVENT-AD, %d participants, %d RBANS visits",
    memory_effect.dt$n_participants, memory_effect.dt$n_visits
  ),
  caption = memory_caption,
  output_key = "memory_figure", slide_key = "memory_slide_figure",
  key_corner = "bottom"
)
mci_paths.v <- save_figures.fn(
  mci.dt, mci_ends.dt, mci_colors.v,
  x_lab = "Years since first UPSIT", y_lab = "Cumulative incidence of MCI (%)",
  title = "**Progression to MCI by baseline smell.**",
  subtitle = sprintf(
    "PREVENT-AD, %d participants, %d with MCI", main_hr.dt$n,
    main_hr.dt$events
  ),
  caption = mci_caption,
  output_key = "mci_figure", slide_key = "mci_slide_figure",
  key_corner = "top"
)

# Each person's yearly change (decline negative), from the delayed memory
# model of 11; time there is in decades
coupling.fit <- coupling.res$fits[[PRIMARY]]
decade.v <- c(upsit = "outcomeupsit:years", rbans = "outcomerbans:years")
random.dt <- as.data.table(ranef(coupling.fit))
coupling.dt <- data.table(
  x = (fixef(coupling.fit)[[decade.v[["upsit"]]]] + random.dt$years_upsit) /
    TRAJ$time_divisor,
  y = (fixef(coupling.fit)[[decade.v[["rbans"]]]] + random.dt$years_rbans) /
    TRAJ$time_divisor
)
slope_cor.dt <- coupling.res$correlations[
  score == PRIMARY & term == "years_upsit~years_rbans"
]
coupling_caption <- sprintf(
  paste(
    "Each point is one person's estimated yearly change in UPSIT and in",
    "the RBANS delayed memory index, from a bivariate linear mixed model",
    "adjusted for age, sex and education (and test version and practice",
    "for RBANS). Correlation of the two rates %.2f (95%% CI %.2f to %.2f,",
    "bootstrap). Estimates are shrunk toward the average, so the points",
    "show the direction, not the strength, of the correlation."
  ),
  slope_cor.dt$estimate, slope_cor.dt$lower, slope_cor.dt$upper
)
coupling_x <- "Smell: UPSIT points a year<br>← declining · rising →"
coupling_y <- "Delayed memory: points a year<br>← declining · rising →"
coupling_note <- sprintf("**Correlation %.2f** [%.2f, %.2f]",
                         slope_cor.dt$estimate, slope_cor.dt$lower,
                         slope_cor.dt$upper)
coupling.plt <- plot_slope_coupling.fn(
  coupling.dt, coupling_x, coupling_y, coupling_note,
  title = "**Smell and memory change together.**",
  subtitle = sprintf("PREVENT-AD, %d people", nrow(coupling.dt)),
  caption = coupling_caption, base_size = BASE_SIZE
)
coupling_slide.plt <- plot_slope_coupling.fn(
  coupling.dt, coupling_x, coupling_y, coupling_note, title = NULL,
  subtitle = NULL,
  caption = NULL, base_size = SLIDE_BASE_SIZE
)
coupling_paths.v <- c(
  vapply(FIGURE_FORMATS, \(fmt) {
    save_plot(
      coupling.plt, paste0(get_data_path("outputs", "coupling_figure"), ".",
                           fmt),
      width = FIGURE_SIZE[1], height = FIGURE_SIZE[2], dpi = DPI
    )
  }, character(1)),
  slide = save_plot(
    coupling_slide.plt,
    paste0(get_data_path("outputs", "coupling_slide_figure"), ".png"),
    width = SLIDE_SIZE[1], height = SLIDE_SIZE[2], dpi = DPI
  )
)

log_section("Part 5: Table")
#' Estimate with 95% CI, or a dash when there is none
#' @param x.dt Zero or one row with estimate, lower, upper
#' @return Character string
estimate_ci.fn <- function(x.dt) {
  if (is.null(x.dt) || nrow(x.dt) == 0) return("–")
  format_effect_ci(x.dt$estimate, x.dt$lower, x.dt$upper, digits = 2)
}
h1.dt <- baseline.res$effects
h1_rows.dt <- rbindlist(lapply(names(SCORE_LABELS), \(score_col) {
  main.dt <- h1.dt[score == score_col & adjustment == "main"]
  data.table(
    section = sprintf(
      "H1. Difference at baseline per %g UPSIT points (index points)", STEP
    ),
    row = sprintf("%s (n = %d)", SCORE_LABELS[[score_col]], main.dt$n),
    main = estimate_ci.fn(main.dt), p = format_p(main.dt$p),
    sensitivity = estimate_ci.fn(
      h1.dt[score == score_col & adjustment == "sensitivity"]
    ),
    pet_sample = "–", amyloid = "–"
  )
}))
h2_rows.dt <- rbindlist(lapply(names(SCORE_LABELS), \(score_col) {
  main.dt <- pick_change.fn(score_col, "main")
  data.table(
    section = sprintf(
      paste(
        "H2 and H4. Difference in annual change per %g UPSIT points",
        "(index points per year)"
      ),
      STEP
    ),
    row = sprintf(
      "%s (n = %d)", SCORE_LABELS[[score_col]], main.dt$n_participants
    ),
    main = estimate_ci.fn(main.dt), p = format_p(main.dt$p),
    sensitivity = estimate_ci.fn(pick_change.fn(score_col, "sensitivity")),
    pet_sample = estimate_ci.fn(pick_change.fn(score_col, "pet_sample")),
    amyloid = estimate_ci.fn(pick_change.fn(score_col, "amyloid"))
  )
}))
hr.fn <- function(adjustment_name) {
  row.dt <- hazard.dt[adjustment == adjustment_name]
  estimate_ci.fn(
    row.dt[, .(estimate = hazard_ratio, lower = lower, upper = upper)]
  )
}
h3_rows.dt <- data.table(
  section = sprintf(
    "H3 and H4. Hazard ratio of MCI per %g UPSIT points", STEP
  ),
  row = sprintf(
    "MCI (n = %d, %d with MCI)", main_hr.dt$n, main_hr.dt$events
  ),
  main = hr.fn("main"), p = format_p(main_hr.dt$p),
  sensitivity = hr.fn("sensitivity"), pet_sample = hr.fn("pet_sample"),
  amyloid = hr.fn("amyloid")
)
table.dt <- rbind(h1_rows.dt, h2_rows.dt, h3_rows.dt)
pet_n <- hazard.dt[adjustment == "amyloid", n]
table_path <- get_data_path("outputs", "rbans_mci_table")
rbans_mci.tbl <- create_pathology_table.fn(
  table.dt,
  column_labels.lst = list(
    main = "Main [95% CI]", p = "p", sensitivity = "Plus APOE ε4 [95% CI]",
    pet_sample = "With PET [95% CI]",
    amyloid = "With PET, plus amyloid [95% CI]"
  ),
  title = "**Memory and progression to MCI by baseline smell.**",
  source_note = sprintf(
    paste(
      "RBANS index scores from the 60-69 norms. Main models adjust for age",
      "at the first UPSIT, sex and education (H1: linear regression; H2:",
      "linear mixed models, also adjusted for test version and practice;",
      "H3: Cox models). With PET: the main model refitted on participants",
      "with a PET scan (%d for MCI); plus amyloid: also adjusted for amyloid",
      "status at the first scan, a median of about 5 years after the first",
      "UPSIT. MCI reviews in the export end on %s. p is for the main model."
    ),
    pet_n, format(mci.res$n$coverage_end, "%B %Y")
  )
)
save_table(
  rbans_mci.tbl, basename(table_path), dirname(table_path),
  formats = get_config("output", "table_formats")
)

log_section("Part 6: Save results")
results.lst <- list(
  table = table.dt,
  memory = list(
    curves = memory.dt, points = memory_points.v, effect = memory_effect.dt,
    caption = memory_caption
  ),
  mci = list(
    curves = mci.dt, points = mci_points.v, caption = mci_caption,
    incidence_at_end = mci_ends.dt[, .(group, time, fit, lower, upper)],
    horizon = list(
      years = HORIZON, incidence = horizon.dt,
      at_risk = at_risk_horizon, events = events_horizon
    )
  ),
  outputs = list(
    memory = unname(memory_paths.v), mci = unname(mci_paths.v),
    coupling = unname(coupling_paths.v),
    table = table_path
  ),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "memory and MCI description")
log_script_end("14_describe_rbans_mci.R", success = TRUE)
