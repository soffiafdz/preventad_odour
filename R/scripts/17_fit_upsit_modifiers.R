#!/usr/bin/env Rscript
# =============================================================================
# 17_fit_upsit_modifiers.R - UPSIT change by APOE e4, sex, and amyloid by
# progression to MCI
# =============================================================================
# Linear mixed models (nlme::lme, REML; Pinheiro & Bates 2000,
# doi:10.1007/b98882) of UPSIT over years since the first test, in the
# trajectory sample, with a random intercept and slope per person.
# 1. APOE e4 and sex: each, and each x years, adjusted for age at the first
#    test (and x years). APOE and sex are centred at their sample means, so
#    the years term is the average yearly change.
# 2. Amyloid by progression to MCI, in people with PET and a known MCI
#    status: four groups (amyloid negative or positive at the first PET, x
#    stayed unimpaired or diagnosed with MCI), each with its own intercept
#    and slope, adjusted for age at the first test and sex (each x years).
#    MCI as in 13_fit_mci_progression.R: the first visit with an MCI
#    diagnosis in the MCI review export; reverted cases are left out.
#    Progression is known only after the fact and the MCI groups are small,
#    so the group slopes are descriptive.
#
# INPUTS:
#   - data/derivatives/smell.rds, pet.rds: from 01 and 04
#   - data/raw/: MCI review export named in the config
# OUTPUTS:
#   - outputs/figures/upsit_amyloid_progression: figure, PDF and PNG, and a
#     slide version
#   - models/results/upsit_modifiers.rds: fits and estimates
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

log_script_start("17_fit_upsit_modifiers.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_upsit_modifiers", default = FALSE
)
output.path <- get_data_path("models", "upsit_modifiers")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("17_fit_upsit_modifiers.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
TRAJ <- get_parameter("trajectories")
MCI <- get_parameter("mci")
MOD <- get_parameter("upsit_modifiers")
MODIFIERS <- unlist(MOD$modifiers)
COVARIATES <- unlist(MOD$covariates)
PROG_COVARIATES <- unlist(MOD$progression_covariates)
ID <- SMELL$id_col
CONTROL <- lmeControl(maxIter = 200, msMaxIter = 200)

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
pet.dt <- read_rds_safe(get_data_path("derivatives", "pet"))$data
mci.dt <- read_csv_safe(
  get_data_path("raw", "mci_csv"),
  select = c(MCI$id_col, MCI$subtype_col),
  colClasses = list(character = MCI$id_col)
)
# Same tests as the trajectory models
smell.dt <- smell.dt[!get(TRAJ$exclude_col) %in% TRAJ$exclude_values]
smell.dt[, n_tests := .N, by = ID]
smell.dt <- smell.dt[n_tests >= TRAJ$min_assessments]
model.dt <- smell.dt[, .(
  id = get(ID), date = get(SMELL$date_col), score = get(SMELL$score_col),
  age = get(SMELL$age_col),
  female = as.numeric(get(SMELL$sex_col) == SMELL$female_value),
  apoe_carrier = as.numeric(get(SMELL$apoe_col) == SMELL$apoe_carrier_value)
)]
setorder(model.dt, id, date)
model.dt[, `:=`(
  subject = .GRP,
  years = as.numeric(date - min(date)) / 365.25,
  age_first = (min(age) - TRAJ$age_centre) / TRAJ$time_divisor
), by = id]
stopifnot(!anyNA(model.dt[, c(MODIFIERS, COVARIATES), with = FALSE]))

#' Estimate and 95% CI of a sum of fixed effects
#' @param fit lme fit
#' @param terms.v Names of the fixed effects to add up
#' @return One-row data.table
combine.fn <- function(fit, terms.v) {
  est.v <- fixef(fit)
  stopifnot(all(terms.v %in% names(est.v)))
  est <- sum(est.v[terms.v])
  se <- sqrt(sum(vcov(fit)[terms.v, terms.v]))
  data.table(estimate = est, lower = est - 1.96 * se,
             upper = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))
}

log_section("Part 2: APOE e4 and sex")
modifier.dt <- copy(model.dt)
person.dt <- modifier.dt[, .SD[1], by = id]
for (col in MODIFIERS) {
  modifier.dt[, (col) := get(col) - mean(person.dt[[col]])]
}
by_time.v <- c(MODIFIERS, COVARIATES)
modifier.fit <- lme(
  reformulate(c("years", by_time.v, paste0("years:", by_time.v)),
              response = "score"),
  random = ~ years | subject, data = as.data.frame(modifier.dt),
  method = "REML", control = CONTROL
)
modifiers.dt <- rbind(
  cbind(term = "Average yearly change", combine.fn(modifier.fit, "years")),
  rbindlist(lapply(MODIFIERS, \(col) {
    cbind(term = col, combine.fn(modifier.fit, paste0("years:", col)))
  }))
)
modifiers.dt[, n := uniqueN(modifier.dt$id)]
for (i in seq_len(nrow(modifiers.dt))) {
  log_info("%s: %.2f UPSIT points a year (95%% CI %.2f to %.2f)",
           modifiers.dt$term[i], modifiers.dt$estimate[i],
           modifiers.dt$lower[i], modifiers.dt$upper[i])
}

log_section("Part 3: Amyloid by progression to MCI")
reverted.v <- unique(mci.dt[
  get(MCI$subtype_col) == MCI$reverted_value, get(MCI$id_col)
])
progressed.v <- setdiff(unique(mci.dt[[MCI$id_col]]), reverted.v)
groups.v <- c("A- stayed", "A+ stayed", "A- MCI", "A+ MCI")
prog.dt <- pet.dt[, .(id = get(get_parameter("pet")$id_col),
                      amyloid = amyloid_positive)][
  model.dt, on = "id", nomatch = NULL
]
prog.dt <- prog.dt[!id %in% reverted.v]
prog.dt[, group := factor(sprintf(
  "%s %s", fifelse(amyloid, "A+", "A-"),
  fifelse(id %in% progressed.v, "MCI", "stayed")
), levels = groups.v)]
prog.dt[, subject := .GRP, by = id]
for (col in setdiff(PROG_COVARIATES, "age_first")) {
  prog.dt[, (col) := get(col) - mean(prog.dt[, .SD[1], by = id][[col]])]
}
# Each group its own intercept and slope (no common reference)
prog.fit <- lme(
  reformulate(c("0", "group", "group:years", PROG_COVARIATES,
                paste0("years:", PROG_COVARIATES)), response = "score"),
  random = ~ years | subject, data = as.data.frame(prog.dt),
  method = "REML", control = CONTROL
)
group_n.dt <- prog.dt[, .(n = uniqueN(id)), by = group]
slope_term.fn <- function(g) paste0("group", g, ":years")
slopes.dt <- rbindlist(lapply(groups.v, \(g) {
  cbind(group = g, n = group_n.dt[group == g, n],
        combine.fn(prog.fit, slope_term.fn(g)))
}))
#' Amyloid difference in yearly change within a progression group
#' @param status "stayed" or "MCI"
#' @return One-row data.table
amyloid_diff.fn <- function(status) {
  pos <- slope_term.fn(paste("A+", status))
  neg <- slope_term.fn(paste("A-", status))
  est.v <- fixef(prog.fit)
  vcov.mat <- vcov(prog.fit)
  est <- est.v[[pos]] - est.v[[neg]]
  se <- sqrt(vcov.mat[pos, pos] + vcov.mat[neg, neg] -
               2 * vcov.mat[pos, neg])
  data.table(status = status, estimate = est, lower = est - 1.96 * se,
             upper = est + 1.96 * se, p = 2 * pnorm(-abs(est / se)))
}
amyloid_diff.dt <- rbind(amyloid_diff.fn("stayed"), amyloid_diff.fn("MCI"))
for (i in seq_len(nrow(slopes.dt))) {
  log_info("%s: %.2f UPSIT points a year (95%% CI %.2f to %.2f), n = %d",
           slopes.dt$group[i], slopes.dt$estimate[i], slopes.dt$lower[i],
           slopes.dt$upper[i], slopes.dt$n[i])
}

log_section("Part 4: Figure")
minus.fn <- function(x) gsub("-", "−", x, fixed = TRUE)
max_years <- quantile(prog.dt[, max(years), by = id]$V1, 0.9, names = FALSE)
curves.dt <- rbindlist(lapply(groups.v, \(g) {
  newdata.df <- data.frame(years = seq(0, max_years, by = 0.1),
                           group = factor(g, levels = groups.v))
  for (col in PROG_COVARIATES) newdata.df[[col]] <- 0
  x.mat <- model.matrix(delete.response(terms(formula(prog.fit))),
                        newdata.df)
  fit.v <- drop(x.mat %*% fixef(prog.fit))
  se.v <- sqrt(rowSums((x.mat %*% vcov(prog.fit)) * x.mat))
  amyloid <- sub(" .*", "", g)
  data.table(
    panel = if (grepl("MCI$", g)) "Diagnosed with MCI" else
      "Stayed cognitively unimpaired",
    group = sprintf("%s (n = %d)", minus.fn(amyloid),
                    group_n.dt[group == g, n]),
    amyloid = amyloid, time = newdata.df$years, fit = fit.v,
    lower = fit.v - 1.96 * se.v, upper = fit.v + 1.96 * se.v
  )
}))
curves.dt[, panel := factor(panel, levels = c(
  "Stayed cognitively unimpaired", "Diagnosed with MCI"
))]
ends.dt <- curves.dt[, .SD[.N], by = .(panel, group)][, label := group]
colors.v <- unlist(lapply(unique(curves.dt$group), \(g) {
  if (startsWith(g, "A+")) manuscript_colors()$pathology_positive else
    manuscript_colors()$pathology_negative
}))
names(colors.v) <- unique(curves.dt$group)
figure_paths.v <- save_figure_pair.fn(
  \(title, subtitle, caption, base_size) {
    plot_pathology_trajectories.fn(
      curves.dt, ends.dt, colors.v, x_lab = "Years since first UPSIT",
      title = title, subtitle = subtitle, caption = caption,
      base_size = base_size
    )
  },
  title = "**Smell over time by amyloid and progression to MCI.**",
  subtitle = sprintf("PREVENT-AD, %d people with PET",
                     uniqueN(prog.dt$id)),
  caption = sprintf(
    paste(
      "Predicted UPSIT with 95%% confidence bands, for age %g at the first",
      "test and average sex. Amyloid at the first PET scan; MCI from the",
      "study's MCI review. Descriptive: progression is known only after the",
      "fact, and the MCI groups are small."
    ),
    TRAJ$age_centre
  ),
  output_key = "progression_figure", slide_key = "progression_slide_figure"
)

log_section("Part 5: Save results")
results.lst <- list(
  fits = list(modifiers = modifier.fit, progression = prog.fit),
  modifiers = modifiers.dt,
  progression = list(slopes = slopes.dt, amyloid_diff = amyloid_diff.dt),
  n = list(participants = uniqueN(model.dt$id),
           progression_participants = uniqueN(prog.dt$id),
           reverted_left_out = length(reverted.v)),
  outputs = unname(figure_paths.v),
  config = list(upsit_modifiers = MOD),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "UPSIT modifiers")
log_script_end("17_fit_upsit_modifiers.R", success = TRUE)
