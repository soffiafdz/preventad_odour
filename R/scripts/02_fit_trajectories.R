#!/usr/bin/env Rscript
# =============================================================================
# 02_fit_trajectories.R - Fit latent class trajectories of UPSIT over time
# =============================================================================
# Latent class trajectories of UPSIT total score (lcmm::hlme; Proust-Lima,
# Philipps & Liquet 2017, J Stat Softw 78(2), doi:10.18637/jss.v078.i02),
# on age and on years since the first assessment (trajectories.time_scales;
# on years, age at the first assessment and its interaction with time are
# covariates common to all classes). Within each class the score is a
# polynomial in time plus independent error. Without random effects this
# is a group-based trajectory model (Nagin & Odgers 2010, Annu Rev Clin
# Psychol 6:109-138, doi:10.1146/annurev.clinpsy.121208.131413); a random
# intercept, or intercept and slope, lets participants vary around their
# class trajectory, so classes are not created only to absorb that
# variation.
# hlme uses each participant's own times, so assessments need not share a
# time grid. Grid: 1 to 5 classes x linear and quadratic x the three
# random-effect structures. Models with more than one class start from many
# random draws around the one-class estimates (lcmm::gridsearch). BIC uses
# the number of participants.
#
# INPUTS:
#   - data/derivatives/smell.rds: from 01_extract_smell.R
# OUTPUTS:
#   - models/results/upsit_trajectories*.rds: fits and comparison table,
#     one file per time scale
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

log_script_start("02_fit_trajectories.R")
config <- load_config()
validate_config(config)
set_seed()

# --- Cache check ---
FORCE_REGENERATE <- get_script_setting(
  "force_regenerate", "fit_trajectories", default = FALSE
)
SCALES <- get_parameter("trajectories", "time_scales")
output_paths.v <- vapply(
  SCALES, \(s) get_data_path("models", "upsit_trajectories", s), character(1)
)
pending.v <- SCALES[FORCE_REGENERATE | !file.exists(output_paths.v)]
if (length(pending.v) == 0) {
  log_info("Outputs exist and force_regenerate=FALSE; skipping")
  log_script_end("02_fit_trajectories.R", success = TRUE)
  quit(status = 0)
}

# --- Configuration ---
SMELL <- get_parameter("smell")
TRAJ <- get_parameter("trajectories")
ID <- SMELL$id_col
stopifnot(all(SCALES %in% c("age", "years_since_baseline")))

log_section("Part 1: Load data")
smell.dt <- read_rds_safe(get_data_path("derivatives", "smell"))$data
smell.dt <- smell.dt[!get(TRAJ$exclude_col) %in% TRAJ$exclude_values]
smell.dt[, n_assessments := .N, by = ID]
smell.dt <- smell.dt[n_assessments >= TRAJ$min_assessments]
# hlme needs a numeric subject identifier
smell.dt[, subject := .GRP, by = ID]
smell.dt[, `:=`(
  age10 = (get(SMELL$age_col) - TRAJ$age_centre) / TRAJ$time_divisor,
  years10 = as.numeric(get(SMELL$date_col) - min(get(SMELL$date_col))) /
    365.25 / TRAJ$time_divisor,
  age_first10 = (min(get(SMELL$age_col)) - TRAJ$age_centre) /
    TRAJ$time_divisor
), by = ID]
setnames(smell.dt, SMELL$score_col, "score")
sample.df <- as.data.frame(smell.dt)

# CJ sorts the grid, so each one-class fit comes before the multi-class
# fits that draw their starting values from it
grid.dt <- CJ(
  degree = TRAJ$degrees,
  random = TRAJ$random_effects,
  n_classes = seq(TRAJ$n_classes[1], TRAJ$n_classes[2])
)

for (SCALE in pending.v) {
  log_section(sprintf("Part 2: Fit models on %s", SCALE))
  # Each scale starts from the configured seed, so its results do not
  # depend on which other scales run in the same session
  set_seed()
  if (SCALE == "age") {
    TIME <- "age10"
    covariates.v <- character()
  } else {
    TIME <- "years10"
    # Participants enter decades apart in age; without age at entry, and its
    # effect on the rate of decline, classes would partly sort them by it
    covariates.v <- c("age_first10", "age_first10:years10")
  }
  time_terms.lst <- list(TIME, c(TIME, sprintf("I(%s^2)", TIME)))
  # ~ -1 is hlme's own default when random is omitted
  random.lst <- list(
    none = ~ -1, intercept = ~ 1, slope = reformulate(TIME)
  )

  fits.lst <- list()
  comparison.lst <- list()
  for (i in seq_len(nrow(grid.dt))) {
    spec <- grid.dt[i]
    name <- sprintf("d%d_%s_g%d", spec$degree, spec$random, spec$n_classes)
    terms.v <- time_terms.lst[[spec$degree]]
    fixed.mod <- reformulate(c(terms.v, covariates.v), response = "score")
    mixture.mod <- reformulate(terms.v)
    random.mod <- random.lst[[spec$random]]

    # Formulas are inlined with bquote() so each stored call is
    # self-contained: predictY() and other lcmm methods read it back
    if (spec$n_classes == 1L) {
      fit <- eval(bquote(hlme(
        .(fixed.mod), random = .(random.mod), subject = "subject", ng = 1,
        data = sample.df, verbose = FALSE
      )))
      one_class.fit <- fit
    } else {
      # A cluster seeded from the config, so the random starts are
      # reproducible for a given number of cores
      cl <- parallel::makeCluster(TRAJ$gridsearch_cores)
      parallel::clusterSetRNGStream(cl, get_seed())
      fit <- eval(bquote(gridsearch(
        hlme(
          .(fixed.mod), mixture = .(mixture.mod), random = .(random.mod),
          subject = "subject", ng = .(spec$n_classes), data = sample.df,
          verbose = FALSE
        ),
        rep = TRAJ$gridsearch_rep[[SCALE]],
        maxiter = TRAJ$gridsearch_maxiter,
        minit = one_class.fit, cl = cl
      )))
      parallel::stopCluster(cl)
    }
    fits.lst[[name]] <- fit

    pprob.dt <- as.data.table(fit$pprob)
    sizes.v <- tabulate(pprob.dt$class, nbins = spec$n_classes)
    # Average posterior probability of the assigned class, per class
    appa.v <- vapply(seq_len(spec$n_classes), \(g) {
      pprob.dt[class == g, mean(get(paste0("prob", g)))]
    }, numeric(1))
    comparison.lst[[name]] <- data.table(
      model = name,
      degree = spec$degree,
      random = spec$random,
      n_classes = spec$n_classes,
      n_participants = uniqueN(sample.df$subject),
      log_lik = fit$loglik,
      bic = fit$BIC,
      conv = fit$conv,
      class_pct = paste(round(100 * sizes.v / sum(sizes.v)), collapse = "/"),
      min_class_n = min(sizes.v),
      appa = paste(sprintf("%.2f", appa.v), collapse = "/"),
      min_appa = min(appa.v)
    )
    log_info(
      "%s: conv %d, logLik %.1f, BIC %.1f, classes %s%%",
      name, fit$conv, fit$loglik, fit$BIC, comparison.lst[[name]]$class_pct
    )
  }
  comparison.dt <- rbindlist(comparison.lst)
  validate_not_empty(comparison.dt, "model comparison")
  if (any(comparison.dt$conv != 1)) {
    log_warn(
      "Not converged: %s",
      paste(comparison.dt[conv != 1, model], collapse = ", ")
    )
  }

  log_section(sprintf("Part 3: Save results on %s", SCALE))
  results.lst <- list(
    fits = fits.lst,
    comparison = comparison.dt,
    subjects = unique(smell.dt[, c(ID, "subject"), with = FALSE]),
    n = list(
      participants = uniqueN(smell.dt[[ID]]),
      assessments = nrow(smell.dt),
      min_assessments = TRAJ$min_assessments,
      excluded_visits = TRAJ$exclude_values
    ),
    config = list(smell = SMELL, trajectories = TRAJ, time_scale = SCALE),
    timestamp = Sys.time()
  )
  write_rds_safe(
    results.lst, output_paths.v[[SCALE]], "UPSIT trajectory fits"
  )
}
log_script_end("02_fit_trajectories.R", success = TRUE)
