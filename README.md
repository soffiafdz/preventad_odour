# Odour identification over time in PREVENT-AD

Analysis code for a study of how the sense of smell changes in older adults
with a family history of Alzheimer's disease, and whether faster loss of
smell goes with Alzheimer's pathology, memory decline and progression to
mild cognitive impairment (MCI).

The code runs on data from the PREVENT-AD cohort, which are not included.
Registered researchers can apply for access at
<https://registeredpreventad.loris.ca> under a data use agreement.

## What it does

1. Groups participants by how their smell test scores (UPSIT) change over
   time, and describes who is in each group.
2. Tests whether smell declines faster with amyloid or tau on PET scans,
   and whether this differs by APOE ε4 or sex.
3. Relates smell to memory (RBANS) and to later MCI diagnoses.

The results end up in a report and a slide deck.

## Running it

```sh
micromamba create -f environment.yml
micromamba activate preventad_odour
Rscript -e 'renv::restore()'

# Scripts in numeric order, 08 last; then the report and slides
for s in R/scripts/0[1-79]_*.R R/scripts/1?_*.R R/scripts/08_*.R; do
  Rscript "$s"
done
(cd reports-src && quarto render)
```

File paths and model settings are in `config/pipeline_config.yaml`; each
script's header says what it reads, fits and writes. Outputs go to
`outputs/`.

## Project notes

- How the data were cleaned and why the models were chosen:
  [`docs/NOTES.md`](docs/NOTES.md).
- Code conventions: [`docs/CONVENTIONS.md`](docs/CONVENTIONS.md); figures
  and slides: [`docs/STYLE.md`](docs/STYLE.md).
- Built from a personal R project template; where this project differs
  from it is listed at the end of `docs/NOTES.md`.
