#!/usr/bin/env Rscript
# =============================================================================
# 16_fit_class_cognition.R - Delayed memory by smell trajectory class
# =============================================================================
# For the classes on class_characteristics.time_scale: RBANS delayed memory
# (60-69 norms) at the visit of the first UPSIT, by linear regression on
# class, and its change over years since the first UPSIT, by a linear mixed
# model (nlme::lme, REML; Pinheiro & Bates 2000, doi:10.1007/b98882) with
# class, years and class x years, a random intercept and slope per person.
# Both adjust for age at the first UPSIT, sex and education (the mixed model
# also for each of them x years, test version and practice), as in
# 12_fit_rbans_change.R. Differences are against the largest class. The
# smaller classes are small, so their estimates are imprecise.
#
# INPUTS:
#   - data/derivatives/smell.rds, rbans.rds: from 01 and 09
#   - data/derivatives/trajectory_classes*.rds: from 03_describe_trajectories.R
# OUTPUTS:
#   - outputs/figures/class_delayed_memory: figure, PDF and PNG, and a slide
#     version
#   - models/results/class_cognition.rds: fits, estimates, curves
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
source(here("R/utils/plotting.R"))
source(here("R/utils/baseline.R"))

log_script_start("16_fit_class_cognition.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_class_cognition", default = FALSE
)
output.path <- get_data_path("models", "class_cognition")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("16_fit_class_cognition.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
RBANS <- get_parameter("rbans")
TRAJ <- get_parameter("trajectories")
SCALE <- get_parameter("class_characteristics")$time_scale
LABELS <- unlist(TRAJ$class_labels[[SCALE]])
COVARIATES <- unlist(get_parameter("class_cognition")$covariates)
VERSIONS <- unlist(RBANS$versions)
VERSION_COLS <- paste0("version_", VERSIONS[-1])
SCORE <- "delayed_memory_index_score_60_69"
stopifnot(SCORE %in% unlist(RBANS$score_cols))

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
rbans.dt <- read_rds_safe(get_data_path("derivatives", "rbans"))$data
classes.dt <- read_rds_safe(
  get_data_path("derivatives", "trajectory_classes", SCALE)
)
person.dt <- build_baseline(smell.dt, rbans.dt)
person.dt <- classes.dt[, .(id = get(SMELL$id_col), class)][
  person.dt, on = "id", nomatch = NULL
]
person.dt[, class := factor(class, levels = LABELS)]
for (col in setdiff(COVARIATES, "age_first")) {
  person.dt[, (col) := get(col) - mean(get(col))]
}
log_info("Participants with a class and a baseline RBANS: %d",
         nrow(person.dt))

setorderv(rbans.dt, c(RBANS$id_col, RBANS$date_col))
visit.dt <- rbans.dt[, .(
  date = get(RBANS$date_col), retest = as.numeric(seq_len(.N) > 1),
  version = get(RBANS$version_col), score = get(SCORE)
), by = .(id = get(RBANS$id_col))]
model.dt <- person.dt[, c("id", "class", "upsit_date", "rbans_date",
                          COVARIATES), with = FALSE][
  visit.dt, on = "id", nomatch = NULL
]
model.dt <- model.dt[date >= rbans_date & !is.na(score)]
model.dt[, `:=`(
  subject = .GRP, years = as.numeric(date - upsit_date) / 365.25
), by = id]
for (v in VERSIONS[-1]) {
  model.dt[, (paste0("version_", v)) := as.numeric(version == v)]
}

log_section("Part 2: Models")
baseline.fit <- lm(
  reformulate(c("class", COVARIATES), response = SCORE),
  data = person.dt
)
change.fit <- lme(
  reformulate(c("years * class", paste0("years:", COVARIATES), COVARIATES,
                "retest", VERSION_COLS), response = "score"),
  random = ~ years | subject, data = as.data.frame(model.dt),
  method = "REML", control = lmeControl(maxIter = 200, msMaxIter = 200)
)

#' Estimate and 95% CI of a sum of coefficients
#' @param est.v,vcov.mat Coefficients and their covariance
#' @param terms.v Names of the coefficients to add up
#' @return One-row data.table
combine.fn <- function(est.v, vcov.mat, terms.v) {
  est <- sum(est.v[terms.v])
  se <- sqrt(sum(vcov.mat[terms.v, terms.v]))
  data.table(estimate = est, lower = est - 1.96 * se,
             upper = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))
}
others.v <- LABELS[-1]
n.dt <- person.dt[, .N, by = class]
estimates.dt <- rbindlist(lapply(LABELS, \(lbl) {
  slope_terms.v <- c("years", if (lbl != LABELS[1]) {
    paste0("years:class", lbl)
  })
  slope.dt <- combine.fn(fixef(change.fit), vcov(change.fit), slope_terms.v)
  row.dt <- data.table(class = lbl, n = n.dt[class == lbl, N],
                       slope = slope.dt$estimate,
                       slope_lower = slope.dt$lower,
                       slope_upper = slope.dt$upper)
  if (lbl %in% others.v) {
    base.dt <- combine.fn(coef(baseline.fit), vcov(baseline.fit),
                          paste0("class", lbl))
    diff.dt <- combine.fn(fixef(change.fit), vcov(change.fit),
                          paste0("years:class", lbl))
    row.dt[, `:=`(
      baseline_diff = base.dt$estimate, baseline_lower = base.dt$lower,
      baseline_upper = base.dt$upper, baseline_p = base.dt$p,
      slope_diff = diff.dt$estimate, slope_diff_lower = diff.dt$lower,
      slope_diff_upper = diff.dt$upper, slope_diff_p = diff.dt$p
    )]
  }
  row.dt
}), fill = TRUE)
for (i in seq_len(nrow(estimates.dt))) {
  log_info("%s: %.2f points a year (95%% CI %.2f to %.2f), n = %d",
           estimates.dt$class[i], estimates.dt$slope[i],
           estimates.dt$slope_lower[i], estimates.dt$slope_upper[i],
           estimates.dt$n[i])
}

log_section("Part 3: Figure")
max_years <- quantile(model.dt[, max(years), by = id]$V1, 0.9,
                      names = FALSE)
curves.dt <- rbindlist(lapply(LABELS, \(lbl) {
  newdata.df <- data.frame(
    years = seq(0, max_years, by = 0.1),
    class = factor(lbl, levels = LABELS), retest = 0
  )
  for (col in COVARIATES) newdata.df[[col]] <- 0
  for (v in VERSION_COLS) newdata.df[[v]] <- 0
  x.mat <- model.matrix(delete.response(terms(formula(change.fit))),
                        newdata.df)
  fit.v <- drop(x.mat %*% fixef(change.fit))
  se.v <- sqrt(rowSums((x.mat %*% vcov(change.fit)) * x.mat))
  data.table(panel = "Delayed memory",
             group = sprintf("%s (n = %d)", lbl, n.dt[class == lbl, N]),
             time = newdata.df$years, fit = fit.v,
             lower = fit.v - 1.96 * se.v, upper = fit.v + 1.96 * se.v)
}))
ends.dt <- curves.dt[, .SD[.N], by = group][, label := group]
colors.v <- setNames(
  unlist(manuscript_colors()[paste0("trajectory_class_", seq_along(LABELS))]),
  unique(curves.dt$group)
)
figure_paths.v <- save_figure_pair.fn(
  \(title, subtitle, caption, base_size) {
    plot_pathology_trajectories.fn(
      curves.dt, ends.dt, colors.v, x_lab = "Years since first UPSIT",
      title = title, subtitle = subtitle, caption = caption,
      base_size = base_size, y_lab = "Predicted delayed memory index"
    )
  },
  title = "**Delayed memory over time by smell trajectory class.**",
  subtitle = sprintf("PREVENT-AD, %d people, %d RBANS visits",
                     uniqueN(model.dt$id), nrow(model.dt)),
  caption = sprintf(
    paste(
      "Predicted RBANS delayed memory index (60-69 norms) with 95%%",
      "confidence bands, for age %g at the first UPSIT, average sex and",
      "education, version A and without the practice gain."
    ),
    TRAJ$age_centre
  ),
  output_key = "class_memory_figure", slide_key = "class_memory_slide_figure"
)

log_section("Part 4: Save results")
results.lst <- list(
  fits = list(baseline = baseline.fit, change = change.fit),
  estimates = estimates.dt,
  curves = curves.dt,
  n = list(participants = uniqueN(model.dt$id), visits = nrow(model.dt)),
  outputs = unname(figure_paths.v),
  config = list(time_scale = SCALE, covariates = COVARIATES),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "delayed memory by class")
log_script_end("16_fit_class_cognition.R", success = TRUE)
