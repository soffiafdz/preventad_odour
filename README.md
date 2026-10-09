# Trajectories of odour identification in PREVENT-AD

Do older adults at familial risk of Alzheimer's disease follow distinct
trajectories of odour identification? This project models longitudinal UPSIT
total scores in PREVENT-AD with group-based trajectory modelling, and relates
odour identification to RBANS memory and to progression to MCI.

## Data access

- Source: PREVENT-AD internal exports, one row per
  visit, review or participant as recorded (smell identification, RBANS,
  demographics, genetics, MCI review), and a PET extract dated 2026-10-06
  prepared within the team. The originals, and the earlier prepared smell
  and RBANS extracts they replace, are kept compressed in `data/archive/`.
- Access: registered researchers apply through
  <https://registeredpreventad.loris.ca> under a data use agreement. The data
  are not part of this repository.
- Data dictionary: none came with the extracts; column meanings confirmed
  with the data provider are listed under Project notes.
- Sharing results: only aggregate results leave the approved team.

## Setup

```sh
micromamba create -f environment.yml
micromamba activate preventad_odour
Rscript -e 'renv::restore()'
```

Conventions: [`docs/CONVENTIONS.md`](docs/CONVENTIONS.md).

## Pipeline

Run from the project root, in order:

| Script | Purpose |
|---|---|
| `R/scripts/01_extract_smell.R` | Keep ID, date, age, visit and UPSIT total score from the smell export, with sex and education from demographics and APOE e4 carrier status from the genotype |
| `R/scripts/02_fit_trajectories.R` | Fit latent class trajectories of UPSIT with `lcmm` on the configured time scale: 1 to 5 classes, linear and quadratic, with no random effects, a random intercept, or a random intercept and slope |
| `R/scripts/03_describe_trajectories.R` | For the configured time scale's selected model: class trajectories with 95% CIs, BIC across the grid, and class characteristics at the first assessment |
| `R/scripts/04_extract_pet.R` | Keep each participant's first PET scan: date, Centiloid, tau meta-ROI SUVR, and amyloid and tau positivity |
| `R/scripts/05_fit_pathology_slopes.R` | Linear mixed models of UPSIT change per year by amyloid and tau (one model per marker, binary and continuous), adjusted for age at the first assessment and sex; sensitivity models add APOE e4 and education |
| `R/scripts/06_describe_pathology_slopes.R` | Figure of predicted UPSIT by amyloid, tau and A/T group, and a table of change per year |
| `R/scripts/07_describe_class_pathology.R` | Amyloid and tau in each trajectory class, descriptive only |
| `R/scripts/08_build_summary_env.R` | Gather every number, table and figure path the report and slides use into `outputs/summary_env.rds` |
| `R/scripts/09_extract_rbans.R` | Keep every RBANS visit: date, age, visit, test version, and the total scale, immediate memory and delayed memory index scores |
| `R/scripts/10_fit_rbans_baseline.R` | H1: RBANS by UPSIT at the first assessment (linear regression), adjusted for age, sex and education; sensitivity adds APOE e4 |
| `R/scripts/11_fit_slope_coupling.R` | H5: correlation of UPSIT and RBANS slopes from a bivariate mixed model, with bootstrap intervals |
| `R/scripts/12_fit_rbans_change.R` | H2: RBANS change per year by baseline UPSIT (mixed models); H4: the same on participants with PET, without and with amyloid status |
| `R/scripts/13_fit_mci_progression.R` | H3: time to MCI by baseline UPSIT (Cox models); H4: the same on participants with PET, without and with amyloid status |
| `R/scripts/14_describe_rbans_mci.R` | Figures of predicted delayed memory and cumulative incidence of MCI at the 25th and 75th percentiles of baseline UPSIT, and a table of H1 to H4 |
| `R/scripts/15_describe_class_characteristics.R` | Who is in each trajectory class (amyloid, tau, APOE ε4, sex, with Wilson 95% CIs), A/T groups and olfactory diagnoses by class, and the smell sample (tests per year and per person) |
| `R/scripts/16_fit_class_cognition.R` | Delayed memory at the first UPSIT and over time by trajectory class |
| `R/scripts/17_fit_upsit_modifiers.R` | UPSIT change by APOE ε4 and sex, and by amyloid within people who stayed unimpaired or were diagnosed with MCI |

`08` also gathers the results of `10` to `17`, which are in the slides but
not yet in the report. Run it after `17`.

`02`, `03` and `07` process every time scale in `trajectories.time_scales`
(`age`, and `years_since_baseline` with age at the first assessment as a
covariate); each scale has its own result files, and a scale whose outputs
exist is skipped. `03`, `07` and `08` skip, with a warning, a scale whose
`trajectories.selected_model` is `pending`.

The report (PDF) and slides render from `reports-src/` into
`outputs/reports/`, after `08`:

```sh
cd reports-src && quarto render
```

The slides exist twice: `slides.qmd` (self-contained HTML, with number
cards) and `slides-pptx.qmd` (PowerPoint, for adapting into other decks,
with the same content in native tables). A change to the slides' content is
made in both. The PowerPoint template `slides-reference.pptx` is built by
`build_slides_reference.py` (Python with python-pptx, outside renv); re-run
it from `reports-src/` only when changing the template.

## Project notes

- Set up from `~/Developer/r_project_template` and brought in line with its
  commit `c48e949` (2026-10-07): the shared utils, `docs/CONVENTIONS.md`,
  `docs/STYLE.md` and the report and slide parts match it. Project-specific
  code is below the `Project-specific` banners in `R/utils/`, and in
  `R/utils/baseline.R`. The slide theme, logos, PowerPoint template,
  `screenshot_slides.sh`, `make_pptx_copy.py` and `docs/STYLE.md` match the
  template at its commit `f9681aa` (2026-10-08); `slides-pptx.qmd` is
  generated from `slides.qmd` by `make_pptx_copy.py`. Group colours in
  `manuscript_colors()` changed from Tol's bright blue and red to his muted
  indigo, wine and olive, so that each is at least 15 (OKLab x 100) from the
  slide brand colours.
- Data are taken as recorded in the internal exports and every derived
  variable (baselines, visit matching, carrier status, follow-up) is computed
  here; derived columns in extracts prepared by others are not used. The
  smell export reproduces the earlier prepared extract exactly (1,541
  visits, with the same scores and ages) and adds 93 rows: 45 visits without
  a score, which are dropped, and 48 scored visits of 54 participants with a
  single scored visit.

- `Study_visit_label` starts with `NAP` for visits in the INTREPAD trial and
  `PRE` for the observational PREVENT-AD cohort. Participants who finished
  the trial continued in the cohort, so 15 have both prefixes.
- `Candidate_Age` is in months in every export; it is divided by 12.
- A few follow-up visits have an UPSIT of 0 and no olfactory diagnosis.
  Forty four-choice items give about 10 by chance and the next lowest score
  is 7, so 0 is treated as missing and these visits are dropped (decided
  2026-10-08). Results with them are in `archive/2026-10-08_upsit_zero/`.
- The genetics export gives the APOE genotype as two allele digits
  separated by a space ("4 3"); carriers have at least one 4. A few
  participants have no genotype.
- UPSIT testing paused for COVID-19 (data provider): the smell export has
  no sessions from 2019 to 2023. Participants with a single scored UPSIT
  started shortly before the pause or after it resumed.
- Seven follow-up assessments have wrong dates in the export, recorded on
  the baseline date or exactly 365 days early. The data
  provider supplied the correct dates, kept outside version control in
  `data/external/smell_date_corrections.csv`. The extraction applies them and
  moves those rows' ages by the same interval (days / 365.25); on all other
  rows, age changes match this rule to within 0.012 years. The export still
  has the original dates.
- Participants scoring in the anosmic range are kept, including those
  anosmic at their first assessment (decision of the PI).
- `diagnosis`, `ccsit_bsit` and the olfactory-condition columns of the smell
  export are not used. Conditions affecting olfaction are not analysed
  (decided 2026-10-08); the question was asked only in 2024-2025.
- Trajectories are fitted with `lcmm::hlme` rather than with `gbmt`.
  `gbmt` compares each participant's scores with the
  group trajectory at every time point in the sample, so it needs all
  participants on one shared time grid; with age as time, each participant
  is seen at about five of over a thousand distinct ages. `hlme` uses each
  participant's own ages.
- PET: amyloid positivity is Centiloid > 20 and tau positivity is meta-ROI
  SUVR > 1.29, as defined by the data provider. Only the first scan per
  participant is used. It falls a median of about 5 years after the first
  UPSIT, so pathology associations describe pathology measured late in the
  smell follow-up. Participants without PET are treated as missing by chance.
- Amyloid and tau are modelled separately: tau positivity is rare (16 of
  the 222 participants with tau at their first scan), too rare to model the
  two jointly or with an A x T interaction. The binary tau model rests on
  those 16 and is low-powered. A/T groups A-T-, A+T- and A+T+ get their own
  slopes; A-T+ (6) is too small. First scans with amyloid but no tau count
  in the amyloid models only.
- The PET extract was re-released: in the first release, the tau meta-ROI
  had been taken from the amyloid (NAV) scan. Results from the first release are in
  `archive/2026-10-06_tau_mislabelled/`.
- Pathology and the trajectory classes are compared descriptively only: the
  classes other than the main one are too small for a model of class
  membership.
- RBANS outcomes are the total scale, immediate memory and delayed memory
  index scores from the 60-69 norms, which are not adjusted for age (data
  provider). Delayed memory is the primary outcome; the other two are
  secondary. Versions A to D are alternate forms; models adjust for the
  version and for practice with a retest indicator (0 at a participant's
  first RBANS, 1 after).
- The baseline for H1 to H4 is each participant's first UPSIT (after the
  trajectory models' visit exclusions) with the RBANS of the same
  `Study_visit_label`: 370 of 380 participants have one, on the same day for
  at least 95% and at most 31 days apart.
- MCI: the export has one row per review, only for participants ever
  diagnosed. The event is the first visit with an MCI diagnosis
  (`initial_MCI_visit_date`). The latest review is from May 2023 while RBANS
  continues to December 2024, so follow-up without MCI ends at the earlier
  of the last RBANS and that latest review, which stands in for the end of
  the export's coverage until the data provider confirms it. Every
  participant is reviewed, so those not in the export were never diagnosed
  (data provider). `MCI_subtype` is 1 amnestic single domain, 2 amnestic
  multiple domain, 3 non-amnestic and 9 reverted to normal (data provider);
  participants with a review coded 9 are excluded from the MCI analyses.
  Reviews coded 4, which the provider's list does not include, are counted
  as MCI.
- H2 and H5 are fitted with `nlme::lme`: in an H2 model with APOE,
  `lcmm::hlme` gave the same estimates and likelihood but inflated numerical
  standard errors. The pathology models in `05` give the same estimates and
  standard errors (within 2%) with both. The H5 slope correlations take
  their intervals from a bootstrap of participants because `nlme`'s
  approximate intervals failed for some scores.
- H4 adjusts for amyloid status at the first PET scan, a median of about 5
  years after the first UPSIT; for some participants who developed MCI the
  scan came after the diagnosis.
