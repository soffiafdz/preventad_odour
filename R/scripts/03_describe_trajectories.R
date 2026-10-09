#!/usr/bin/env Rscript
# =============================================================================
# 03_describe_trajectories.R - Describe the selected UPSIT trajectory model
# =============================================================================
# For the configured time scale and its selected model
# (trajectories.selected_model): class mean trajectories with 95% CIs
# (lcmm::predictY), drawn only over the times each class's members cover;
# BIC across the model grid; and the classes' characteristics at the first
# assessment. On years since baseline, curves are for a participant of the
# sample's mean age at the first assessment. Participants are assigned to
# the class with the highest posterior probability.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
#   - models/results/upsit_trajectories*.rds: from 02_fit_trajectories.R
# OUTPUTS, one set per time scale:
#   - outputs/figures/: class trajectories and BIC comparison, PDF and PNG;
#     class trajectories for slides, PNG
#   - outputs/tables/: class characteristics, HTML
#   - models/results/trajectory_description*.rds: the numbers behind them
#   - data/derivatives/trajectory_classes*.rds: class per participant
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

log_script_start("03_describe_trajectories.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "describe_trajectories", default = FALSE
)
SCALES <- get_parameter("trajectories", "time_scales")
output_paths.v <- vapply(
  SCALES, \(s) get_data_path("models", "trajectory_description", s),
  character(1)
)
pending.v <- SCALES[FORCE_REGENERATE | !file.exists(output_paths.v)]
if (length(pending.v) == 0) {
  log_info("Outputs exist and force_regenerate=FALSE; skipping")
  log_script_end("03_describe_trajectories.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
TRAJ <- get_parameter("trajectories")
ID <- SMELL$id_col
FIGURE_FORMATS <- get_config("output", "figure_format")
DPI <- get_config("output", "dpi")
FIGURE_SIZE <- get_config("output", "figure_size")
BASE_SIZE <- get_config("output", "base_size")
SLIDE_SIZE <- get_config("output", "slide_figure_size")
SLIDE_BASE_SIZE <- get_config("output", "slide_base_size")

for (SCALE in pending.v) {
  log_section(sprintf("Time scale: %s", SCALE))
  if (identical(TRAJ$selected_model[[SCALE]], "pending")) {
    log_warn("%s: model selection pending; skipped", SCALE)
    next
  }
  log_section("Part 1: Load data and selected model")
  SELECTED <- require_output(TRAJ$selected_model[[SCALE]], "selected model")
  LABELS <- require_output(TRAJ$class_labels[[SCALE]], "class labels")
  trajectories.res <- read_rds_safe(
    get_data_path("models", "upsit_trajectories", SCALE)
  )
  fit <- require_output(trajectories.res$fits[[SELECTED]], SELECTED)
  stopifnot(fit$conv == 1, fit$ng == length(LABELS))

  smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
  # Same assessments as in 02: excluded visits dropped, fitted participants
  smell.dt <- smell.dt[!get(TRAJ$exclude_col) %in% TRAJ$exclude_values]
  smell.dt <- trajectories.res$subjects[smell.dt, on = ID, nomatch = NULL]
  smell.dt[, `:=`(
    age = get(SMELL$age_col),
    score = get(SMELL$score_col),
    years = as.numeric(get(SMELL$date_col) - min(get(SMELL$date_col))) / 365.25
  ), by = ID]

  log_section("Part 2: Class mean trajectories")
  if (SCALE == "age") {
    smell.dt[, time := age]
    time.v <- seq(floor(min(smell.dt$time)), ceiling(max(smell.dt$time)), 0.25)
    newdata.df <- data.frame(
      age10 = (time.v - TRAJ$age_centre) / TRAJ$time_divisor
    )
    time_var <- "age10"
    label_row <- which(time.v == TRAJ$age_centre)
    x_lab <- "Age (years)"
    time_text <- "in age"
    range_text <- "ages"
    reference_text <- ""
  } else {
    smell.dt[, time := years]
    time.v <- seq(0, ceiling(max(smell.dt$time)), 0.1)
    reference_age <- smell.dt[, min(age), by = ID][, mean(V1)]
    newdata.df <- data.frame(
      years10 = time.v / TRAJ$time_divisor,
      age_first10 = (reference_age - TRAJ$age_centre) / TRAJ$time_divisor
    )
    time_var <- "years10"
    label_row <- 1L
    x_lab <- "Years since first UPSIT"
    time_text <- "in years since the first assessment, adjusted for age at it"
    range_text <- "years of follow-up"
    reference_text <- sprintf(
      " Curves are for age %.1f at the first assessment (sample mean).",
      reference_age
    )
  }
  predicted <- predictY(fit, newdata.df, var.time = time_var, draws = TRUE)$pred
  # Classes are labelled in order of predicted score at label_row (the
  # centring age, or baseline)
  at_start.v <- predicted[label_row, seq_len(fit$ng)]
  label.v <- LABELS[order(order(-at_start.v))]

  pprob.dt <- as.data.table(fit$pprob)
  pprob.dt[, posterior := as.matrix(.SD)[cbind(.I, class)],
    .SDcols = paste0("prob", seq_len(fit$ng))
  ]
  smell.dt <- pprob.dt[, .(subject, class, posterior)][smell.dt, on = "subject"]
  smell.dt[, class := factor(label.v[class], levels = LABELS)]

  # One row per participant: assigned class and the probability of each class
  classes.dt <- trajectories.res$subjects[pprob.dt, on = "subject"]
  classes.dt[, class := factor(label.v[class], levels = LABELS)]
  prob_names.v <- paste0("prob_", gsub(" ", "_", tolower(LABELS)))
  setnames(
    classes.dt, paste0("prob", seq_len(fit$ng)),
    prob_names.v[match(label.v, LABELS)]
  )
  classes.dt <- classes.dt[, c(ID, "class", "posterior", prob_names.v),
    with = FALSE
  ]
  write_rds_safe(
    classes.dt, get_data_path("derivatives", "trajectory_classes", SCALE),
    "class per participant"
  )

  curves.dt <- rbindlist(lapply(seq_len(fit$ng), \(g) {
    data.table(
      time = time.v,
      class = factor(label.v[g], levels = LABELS),
      fit = predicted[, paste0("Ypred_class", g)],
      lower = predicted[, paste0("lower.Ypred_class", g)],
      upper = predicted[, paste0("upper.Ypred_class", g)]
    )
  }))
  # Draw each class only over the times its members were assessed at
  ranges.dt <- smell.dt[, .(min_t = min(time), max_t = max(time)), by = class]
  curves.dt <- ranges.dt[curves.dt, on = "class"][
    time >= min_t & time <= max_t, !c("min_t", "max_t")
  ]

  log_section("Part 3: Class characteristics")
  person.dt <- smell.dt[order(get(ID), age), .(
    class = class[1],
    posterior = posterior[1],
    baseline_age = age[1],
    female = get(SMELL$sex_col)[1] == SMELL$female_value,
    education = get(SMELL$education_col)[1],
    apoe_carrier = get(SMELL$apoe_col)[1] == SMELL$apoe_carrier_value,
    n_assessments = .N,
    follow_up = as.numeric(diff(range(get(SMELL$date_col)))) / 365.25
  ), by = ID]

  #' Summary statistics of one group of participants
  #' @param group.dt Rows of person.dt
  #' @param group Group label
  #' @return One-row data.table
  summarise_group.fn <- function(group.dt, group) {
    data.table(
      group = group,
      n = nrow(group.dt),
      posterior_mean = mean(group.dt$posterior),
      age_mean = mean(group.dt$baseline_age),
      age_sd = sd(group.dt$baseline_age),
      n_female = sum(group.dt$female),
      education_mean = mean(group.dt$education),
      education_sd = sd(group.dt$education),
      n_apoe_carrier = sum(group.dt$apoe_carrier),
      assessments_median = median(group.dt$n_assessments),
      assessments_min = min(group.dt$n_assessments),
      assessments_max = max(group.dt$n_assessments),
      follow_up_mean = mean(group.dt$follow_up),
      follow_up_sd = sd(group.dt$follow_up)
    )
  }
  summary.dt <- rbind(
    rbindlist(lapply(LABELS, \(lab) {
      summarise_group.fn(person.dt[class == lab], lab)
    })),
    summarise_group.fn(person.dt, "Total")
  )
  n_total <- nrow(person.dt)
  summary.dt[, pct := 100 * n / n_total]

  n_pct.fn <- function(count, n) sprintf("%d (%.0f%%)", count, 100 * count / n)
  mean_sd.fn <- function(m, s) sprintf("%.1f (%.1f)", m, s)
  table.dt <- data.table(characteristic = c(
    "Participants, n (%)",
    "Posterior probability of class, mean",
    "Age at first assessment, mean (SD)",
    "Women, n (%)",
    "Education, years, mean (SD)",
    "APOE \u03b54 carriers, n (%)",
    "Assessments, median (range)",
    "Follow-up, years, mean (SD)"
  ))
  for (grp in summary.dt$group) {
    s <- summary.dt[group == grp]
    set(table.dt, j = grp, value = c(
      n_pct.fn(s$n, n_total),
      sprintf("%.2f", s$posterior_mean),
      mean_sd.fn(s$age_mean, s$age_sd),
      n_pct.fn(s$n_female, s$n),
      mean_sd.fn(s$education_mean, s$education_sd),
      n_pct.fn(s$n_apoe_carrier, s$n),
      sprintf(
        "%g (%d-%d)", s$assessments_median, s$assessments_min, s$assessments_max
      ),
      mean_sd.fn(s$follow_up_mean, s$follow_up_sd)
    ))
  }

  log_section("Part 4: Figures and table")
  comparison.dt <- copy(trajectories.res$comparison)
  comparison.dt[, `:=`(
    selected = model == SELECTED,
    degree = factor(degree, 1:2, c("Linear", "Quadratic")),
    random = factor(
      random, c("none", "intercept", "slope"),
      c("None", "Intercept", "Intercept and slope")
    )
  )]
  selected.dt <- comparison.dt[selected == TRUE]
  model_text <- sprintf(
    "%s trajectories %s, random effects: %s, %d classes",
    selected.dt$degree, time_text, tolower(selected.dt$random),
    selected.dt$n_classes
  )

  # Legend labels carry each class's size
  class_n.v <- summary.dt[match(LABELS, group), n]
  legend.v <- sprintf(
    "%s (n = %d, %.0f%%)", LABELS, class_n.v, 100 * class_n.v / n_total
  )
  class_colors.v <- setNames(
    unlist(manuscript_colors()[paste0("trajectory_class_", seq_len(fit$ng))]),
    legend.v
  )
  plot_obs.dt <- copy(smell.dt)[, class := factor(class, LABELS, legend.v)]
  plot_curves.dt <- copy(curves.dt)[, class := factor(class, LABELS, legend.v)]
  trajectory.plt <- plot_class_trajectories.fn(
    plot_obs.dt, plot_curves.dt, class_colors.v, ID, SMELL$score_range, x_lab,
    title = sprintf(
      "**UPSIT trajectories %s in %d classes.**",
      if (SCALE == "age") "with age" else "over the study", fit$ng
    ),
    subtitle = sprintf(
      "PREVENT-AD, %d participants, %d UPSIT assessments",
      n_total, nrow(smell.dt)
    ),
    caption = sprintf(
      paste(
        "Thick lines: class means with 95%% confidence bands, drawn over the",
        "%s each class covers. Faint lines: individual participants,",
        "coloured by their most likely class. Model: %s (lcmm). INTREPAD",
        "3-month visits excluded.%s"
      ),
      range_text, tolower(model_text), reference_text
    ),
    base_size = BASE_SIZE
  )
  # The slide heading and notes carry the title and caption
  trajectory_slide.plt <- plot_class_trajectories.fn(
    plot_obs.dt, plot_curves.dt, class_colors.v, ID, SMELL$score_range, x_lab,
    title = NULL, subtitle = NULL, caption = NULL, base_size = SLIDE_BASE_SIZE
  )

  random_colors.v <- setNames(
    unlist(manuscript_colors()[
      c("random_none", "random_intercept", "random_slope")
    ]),
    levels(comparison.dt$random)
  )
  bic.plt <- plot_bic_comparison.fn(
    comparison.dt, random_colors.v,
    title = "**Choosing the number of trajectory classes.**",
    subtitle = sprintf(
      "BIC for %d to %d classes under %d model specifications",
      min(comparison.dt$n_classes), max(comparison.dt$n_classes),
      uniqueN(comparison.dt, by = c("degree", "random"))
    ),
    caption = sprintf(
      paste(
        "Circled: selected model (%s; BIC %.0f). Random effects let",
        "participants vary around their class trajectory instead of creating",
        "extra classes for that variation."
      ),
      tolower(model_text), selected.dt$bic
    ),
    base_size = BASE_SIZE
  )

  figure_paths.v <- save_plot(
    trajectory_slide.plt,
    paste0(get_data_path("outputs", "trajectory_slide_figure", SCALE), ".png"),
    width = SLIDE_SIZE[1], height = SLIDE_SIZE[2], dpi = DPI
  )
  for (fmt in FIGURE_FORMATS) {
    figure_paths.v <- c(
      figure_paths.v,
      save_plot(
        trajectory.plt,
        paste0(get_data_path("outputs", "trajectory_figure", SCALE), ".", fmt),
        width = FIGURE_SIZE[1], height = FIGURE_SIZE[2], dpi = DPI
      ),
      save_plot(
        bic.plt,
        paste0(get_data_path("outputs", "bic_figure", SCALE), ".", fmt),
        width = FIGURE_SIZE[1], height = FIGURE_SIZE[2], dpi = DPI
      )
    )
  }

  table_path <- get_data_path("outputs", "class_table", SCALE)
  class.tbl <- create_class_table.fn(
    table.dt,
    title = "**Characteristics of the UPSIT trajectory classes.**",
    source_note = sprintf(
      paste(
        "Model: %s. Participants are assigned to their most likely class;",
        "characteristics are at the first assessment."
      ),
      tolower(model_text)
    )
  )
  save_table(
    class.tbl, basename(table_path), dirname(table_path),
    formats = get_config("output", "table_formats")
  )

  log_section("Part 5: Save results")
  results.lst <- list(
    class_summary = summary.dt,
    class_table = table.dt,
    curves = curves.dt,
    comparison = comparison.dt,
    outputs = list(figures = figure_paths.v, table = table_path),
    n = list(
      participants = nrow(person.dt),
      assessments = nrow(smell.dt),
      per_class = setNames(summary.dt$n, summary.dt$group)
    ),
    config = list(
      smell = SMELL, trajectories = TRAJ, time_scale = SCALE,
      selected_model = SELECTED
    ),
    timestamp = Sys.time()
  )
  write_rds_safe(results.lst, output_paths.v[[SCALE]], "trajectory description")
}
log_script_end("03_describe_trajectories.R", success = TRUE)
