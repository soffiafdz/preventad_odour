# Data and analysis notes

What the exports contain, how they are cleaned, and why the models are what
they are. Facts marked "data provider" were confirmed by them; no data
dictionary came with the exports.

## Data

- Inputs: PREVENT-AD internal exports of smell tests, RBANS, demographics,
  genetics and MCI reviews, and a PET extract prepared within the team
  (October 2026). The config reads them as `data/raw/smell.csv` and so on,
  links to the files as released.
- Every derived variable (baselines, visit matching, carrier status,
  follow-up) is computed here from the exports as recorded; derived columns
  in extracts prepared by others are not used.
- Ages (`Candidate_Age`) are in months and are divided by 12.
- Visits labelled `NAP…` belong to the INTREPAD trial and `PRE…` to the
  observational cohort; participants who finished the trial continued in
  the cohort. The trial's 3-month retest (`NAPFU03`) is left out of the
  trajectory models.

### Smell (UPSIT)

- Visits without a score are dropped. A score of 0 is treated as missing:
  forty four-choice items give about 10 by chance.
- Seven follow-up visits had wrong dates in the export. The data provider
  supplied the correct ones, kept outside version control in
  `data/external/smell_date_corrections.csv`; ages move by the same
  interval.
- Participants scoring in the anosmic range are kept, including at their
  first test (decision of the PI).
- Testing paused for COVID-19 (data provider): there are no tests from 2019
  to 2023.
- Conditions affecting smell were asked about only in 2024-2025 and are not
  analysed. The recorded olfactory diagnosis only describes the trajectory
  groups.

### Other measures

- APOE ε4: the genotype is two allele digits ("4 3"); a carrier has at
  least one 4.
- PET: amyloid positive is Centiloid > 20 and tau positive is meta-ROI SUVR
  > 1.29 (data provider). Only each participant's first scan is used; it
  comes a median of about 5 years after the first smell test.
- RBANS: index scores from the 60-69 norms, not adjusted for age (data
  provider). Delayed memory is the main outcome. Versions A to D are
  alternate forms.
- Memory at baseline is the RBANS from the same visit as the first smell
  test.
- MCI: the export lists only participants ever diagnosed, and everyone is
  reviewed (data provider). The event is the first visit with an MCI
  diagnosis. Cases that reverted to normal (subtype 9) are left out. Reviews
  end in May 2023, so follow-up without MCI ends there at the latest.

## Models

- Smell trajectories use latent class mixed models (`lcmm::hlme`), which
  follow each participant at their own ages; `gbmt` would need everyone on
  one shared time grid. The number of groups is chosen by BIC, how clearly
  people are assigned, and group size; the reasoning is in the config under
  `trajectories.selected_model`.
- The smaller trajectory groups are too small to model, so who is in them
  is described, not tested.
- Amyloid and tau are modelled separately: too few people are tau positive
  for a joint model.
- Memory models adjust for RBANS version and for practice (first RBANS or
  later).
- Memory, the smell-memory correlation and the group and modifier models
  use `nlme::lme`: `lcmm::hlme` gave inflated standard errors in one memory
  model. The smell-memory correlation takes its intervals from a bootstrap
  because `nlme`'s approximate intervals failed.
- Progression to MCI is known only after the fact, so smell decline by
  amyloid and later MCI is descriptive.

## Differences from the project template

- The project follows the template's utils, conventions and report parts
  (commit `c48e949`) and its slide theme and scripts (commit `f9681aa`).
  Project code is below the `Project-specific` banners in `R/utils/` and in
  `R/utils/baseline.R`.
- Group colours are Tol's muted indigo, wine and olive, to stay clear of the
  slide brand colours.
- Since `f9681aa`: slide figures are as wide as the space under a slide
  title, PET-negative groups are grey, and part dividers balance their
  lines.
