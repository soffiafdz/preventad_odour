#!/usr/bin/env Rscript
# =============================================================================
# 15_describe_class_characteristics.R - Who is in each trajectory class
# =============================================================================
# Descriptive only, for the classes on class_characteristics.time_scale: the
# share of each class that is amyloid positive and tau positive (first PET),
# APOE e4 carrier and female, with Wilson 95% CIs (prop.test without
# continuity correction); A/T groups by class; and olfactory diagnoses from
# the UPSIT norms at each person's first and last test. The classes other
# than the main one are small, so no tests are reported. Also describes the
# smell sample: tests per calendar year and tests per person.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
#   - data/derivatives/pet.rds: from 04_extract_pet.R
#   - data/derivatives/trajectory_classes*.rds: from 03_describe_trajectories.R
# OUTPUTS:
#   - outputs/figures/: class profile, A/T groups, olfactory diagnoses and
#     the smell sample, each PDF and PNG with a slide version
#   - models/results/class_characteristics.rds: the numbers behind them
# =============================================================================

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R/utils/logging.R"))
source(here("R/utils/config.R"))
source(here("R/utils/data_io.R"))
source(here("R/utils/validation.R"))
source(here("R/utils/plotting.R"))

log_script_start("15_describe_class_characteristics.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "describe_class_characteristics", default = FALSE
)
output.path <- get_data_path("models", "class_characteristics")
if (!FORCE_REGENERATE && file.exists(output.path)) {
  log_info("Output exists and force_regenerate=FALSE; skipping")
  log_script_end("15_describe_class_characteristics.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
PET <- get_parameter("pet")
TRAJ <- get_parameter("trajectories")
SCALE <- get_parameter("class_characteristics")$time_scale
LABELS <- unlist(TRAJ$class_labels[[SCALE]])
ID <- SMELL$id_col
# Olfactory diagnoses as exported, from best to worst
DIAGNOSES <- c(
  Normosmia = "diagnosis_normosmia", `Mild Microsmia` = "diagnosis_mild",
  `Moderate Microsmia` = "diagnosis_moderate",
  `Severe Microsmia` = "diagnosis_severe",
  `Total Anosmia` = "diagnosis_anosmia"
)
# Shown in sentence case
DIAGNOSIS_LABELS <- setNames(
  c("Normosmia", "Mild microsmia", "Moderate microsmia", "Severe microsmia",
    "Anosmia"),
  names(DIAGNOSES)
)
AT_GROUPS <- c(
  `A-T-` = "pathology_negative", `A-T+` = "pathology_tau_only",
  `A+T-` = "pathology_amyloid_only", `A+T+` = "pathology_positive"
)
minus.fn <- function(x) gsub("-", "−", x, fixed = TRUE)

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
pet.dt <- read_rds_safe(get_data_path("derivatives", "pet"))$data
classes.dt <- read_rds_safe(
  get_data_path("derivatives", "trajectory_classes", SCALE)
)
# The trajectory sample: the classes' own tests
smell.dt <- smell.dt[!get(TRAJ$exclude_col) %in% TRAJ$exclude_values]
smell.dt <- smell.dt[get(ID) %in% classes.dt[[ID]]]
setorderv(smell.dt, c(ID, SMELL$date_col))
stopifnot(setequal(unique(smell.dt[[ID]]), classes.dt[[ID]]))

person.dt <- smell.dt[, .(
  female = get(SMELL$sex_col)[1] == SMELL$female_value,
  apoe_carrier = get(SMELL$apoe_col)[1] == SMELL$apoe_carrier_value,
  first_diagnosis = get(SMELL$diagnosis_col)[1],
  last_diagnosis = get(SMELL$diagnosis_col)[.N]
), by = ID]
person.dt <- classes.dt[, c(ID, "class"), with = FALSE][person.dt, on = ID]
person.dt <- pet.dt[, c(
  ID, "amyloid_positive", "tau_positive"
), with = FALSE][person.dt, on = ID]
person.dt[, class := factor(class, levels = LABELS)]
stopifnot(all(unique(c(person.dt$first_diagnosis, person.dt$last_diagnosis))
              %in% names(DIAGNOSES)))

log_section("Part 2: Profile of each class")
#' Count, share and Wilson 95% CI of a yes/no characteristic by class
#' @param col Logical column of person.dt
#' @param panel Panel label
#' @return data.table, one row per class
proportion.fn <- function(col, panel) {
  person.dt[!is.na(get(col)), {
    x <- sum(get(col))
    n <- .N
    ci.v <- if (n > 0) prop.test(x, n, correct = FALSE)$conf.int else NA
    .(panel = panel, x = x, n = n, pct = 100 * x / n,
      lower = 100 * ci.v[1], upper = 100 * ci.v[2])
  }, by = .(group = class)]
}
props.dt <- rbind(
  proportion.fn("amyloid_positive", "Amyloid positive"),
  proportion.fn("tau_positive", "Tau positive"),
  proportion.fn("apoe_carrier", "APOE ε4 carrier"),
  proportion.fn("female", "Female")
)
props.dt[, panel := factor(panel, levels = unique(panel))]
class_colors.v <- setNames(
  unlist(manuscript_colors()[paste0("trajectory_class_", seq_along(LABELS))]),
  LABELS
)
profile_paths.v <- save_figure_pair.fn(
  \(title, subtitle, caption, base_size) {
    plot_class_proportions.fn(
      props.dt, class_colors.v, title, subtitle, caption, base_size
    )
  },
  title = "**Who is in each smell trajectory class.**",
  subtitle = sprintf("PREVENT-AD, %d people", nrow(person.dt)),
  caption = paste(
    "Share of each class with 95% Wilson confidence intervals; counts",
    "above. Amyloid and tau at the first PET scan, in those with PET.",
    "Descriptive: the smaller classes are too small to test."
  ),
  output_key = "class_profile_figure", slide_key = "class_profile_slide_figure"
)

log_section("Part 3: A/T groups and olfactory diagnoses")
person.dt[, at_group := fcase(
  is.na(tau_positive), NA_character_,
  amyloid_positive & tau_positive, "A+T+",
  amyloid_positive, "A+T-",
  tau_positive, "A-T+",
  default = "A-T-"
)]
#' Counts of each level by class, with every level present
#' @param col Column of person.dt holding the level
#' @param levels.v Levels in display order
#' @return data.table with group, level, N
composition.fn <- function(col, levels.v) {
  counts.dt <- person.dt[!is.na(get(col)), .N, by = .(group = class,
                                                      level = get(col))]
  counts.dt <- counts.dt[CJ(group = factor(LABELS, levels = LABELS),
                            level = levels.v), on = .(group, level)]
  counts.dt[is.na(N), N := 0L]
  counts.dt[]
}
at.dt <- composition.fn("at_group", names(AT_GROUPS))
at_n.dt <- at.dt[, .(n = sum(N)), by = group]
at.dt[, `:=`(
  group = factor(sprintf("%s<br>(n = %d)", group,
                         at_n.dt$n[match(group, at_n.dt$group)]),
                 levels = sprintf("%s<br>(n = %d)", LABELS, at_n.dt$n)),
  level = factor(minus.fn(level), levels = minus.fn(names(AT_GROUPS)))
)]
at_fill.v <- setNames(unlist(manuscript_colors()[AT_GROUPS]),
                      minus.fn(names(AT_GROUPS)))
at_paths.v <- save_figure_pair.fn(
  \(title, subtitle, caption, base_size) {
    plot_stacked_shares.fn(
      at.dt, at_fill.v, dark.v = minus.fn(c("A-T-", "A+T+", "A+T-")),
      title, subtitle, caption, base_size, horizontal = TRUE
    )
  },
  title = "**Amyloid and tau by smell trajectory class.**",
  subtitle = sprintf("PREVENT-AD, %d people with amyloid and tau PET",
                     sum(at_n.dt$n)),
  caption = sprintf(
    "First PET scan; positive above %g Centiloids and %g meta-ROI SUVR.",
    PET$amyloid_cutoff, PET$tau_cutoff
  ),
  output_key = "class_at_figure", slide_key = "class_at_slide_figure"
)

diagnosis.dt <- rbind(
  composition.fn("first_diagnosis", names(DIAGNOSES))[, panel := "First test"],
  composition.fn("last_diagnosis", names(DIAGNOSES))[, panel := "Last test"]
)
class_n.dt <- person.dt[, .N, by = .(group = class)]
# One panel per class, the first and last test side by side within it
class_panel.v <- sprintf("%s (n = %d)", LABELS,
                         class_n.dt$N[match(LABELS, class_n.dt$group)])
diagnosis.dt[, `:=`(
  panel = factor(class_panel.v[match(group, LABELS)], levels = class_panel.v),
  group = factor(sub(" test$", "", panel),
                 levels = c("First", "Last")),
  level = factor(DIAGNOSIS_LABELS[level], levels = DIAGNOSIS_LABELS)
)]
diagnosis_fill.v <- setNames(unlist(manuscript_colors()[DIAGNOSES]),
                             DIAGNOSIS_LABELS)
diagnosis_paths.v <- save_figure_pair.fn(
  \(title, subtitle, caption, base_size) {
    plot_stacked_shares.fn(
      diagnosis.dt, diagnosis_fill.v,
      dark.v = DIAGNOSIS_LABELS[c("Severe Microsmia", "Total Anosmia")],
      title, subtitle, caption, base_size
    )
  },
  title = "**Olfactory diagnoses by smell trajectory class.**",
  subtitle = sprintf("PREVENT-AD, %d people", nrow(person.dt)),
  caption = paste(
    "Diagnoses from the UPSIT norms, as recorded with each person's first",
    "and last test."
  ),
  output_key = "class_diagnosis_figure",
  slide_key = "class_diagnosis_slide_figure"
)

log_section("Part 4: The smell sample")
years.v <- year(smell.dt[[SMELL$date_col]])
year_range.v <- seq(min(years.v), max(years.v))
per_year.dt <- data.table(x = factor(years.v, levels = year_range.v))
per_year.dt <- per_year.dt[, .N, by = x]
per_year.dt <- per_year.dt[
  CJ(x = factor(year_range.v, levels = year_range.v)), on = "x"
]
per_year.dt[is.na(N), N := 0L]
per_person.dt <- smell.dt[, .N, by = ID][, .N, by = .(x = N)][order(x)]
per_person.dt[, x := factor(x)]
sample.dt <- rbind(
  per_year.dt[, panel := "UPSIT tests per year"],
  per_person.dt[, panel := "People by number of tests"]
)
sample.dt[, panel := factor(panel, levels = unique(panel))]
# Every other year labelled, so the years fit flat under the bars
odd_years.v <- as.character(year_range.v[year_range.v %% 2 == 1])
sample_paths.v <- save_figure_pair.fn(
  \(title, subtitle, caption, base_size) {
    plot_counts.fn(sample.dt, title, subtitle, caption, base_size,
                   x_labels = \(x) fifelse(x %in% odd_years.v, "", x))
  },
  title = "**UPSIT tests in PREVENT-AD.**",
  subtitle = sprintf("%d people with 2 or more tests, %d tests",
                     uniqueN(smell.dt[[ID]]), nrow(smell.dt)),
  caption = "Smell testing paused from 2019 to 2023.",
  output_key = "sample_figure", slide_key = "sample_slide_figure"
)

log_section("Part 5: Save results")
results.lst <- list(
  profile = props.dt,
  at_groups = at.dt,
  diagnoses = diagnosis.dt,
  sample = sample.dt,
  n = list(participants = nrow(person.dt), tests = nrow(smell.dt)),
  outputs = list(
    profile = unname(profile_paths.v), at_groups = unname(at_paths.v),
    diagnoses = unname(diagnosis_paths.v), sample = unname(sample_paths.v)
  ),
  config = list(time_scale = SCALE),
  timestamp = Sys.time()
)
write_rds_safe(results.lst, output.path, "class characteristics")
log_script_end("15_describe_class_characteristics.R", success = TRUE)
