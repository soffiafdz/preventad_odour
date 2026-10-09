# Conventions

How this repository is set up and how its code is written. The aim is that
anyone with access to the data can rebuild the software environment, run the
pipeline, and trace every reported number back to the code that produced it.
Where this project departs from these conventions, the README says so under
"Project notes".

## Principles

Every number in a report, manuscript or poster is read from a result file
written by the pipeline. Nothing is typed into text or code by hand, so a
change upstream changes the reported number too.

Code is written against the documented structure of the data: variable
names, codings, units and valid ranges come from the release's data
dictionary or from inspecting the data. Validation checks cite their source
in a comment. Because this code is public and the data are not, checks use
documented ranges rather than observed extremes, which could point to an
individual participant, and the dictionary is cited, not copied.

Missing inputs and empty results stop the run with an error instead of
being skipped silently. Participant data never enter version control; only
code, configuration and documentation are tracked.

Citations are checked against the DOI or the publisher before they enter
`references.bib`.

## Environment

R, Quarto, the compilers and the system libraries come from a conda-forge
environment (`environment.yml`, created with micromamba, mamba or conda).
R packages and their exact versions are recorded by renv in `renv.lock`.
The conda environment pins R; renv pins the packages. On macOS, conda-forge
R installs packages from source, which is why the compilers are part of the
environment.

```sh
micromamba create -f environment.yml
micromamba activate <env-name>
Rscript -e 'renv::restore()'
```

renv activates itself through `.Rprofile` when R starts in the project
root. Packages come from Posit Package Manager, which serves prebuilt
binaries on Linux. renv records every package the code uses; `DESCRIPTION`
lists only packages that no code calls directly, such as `lintr`. After
adding or removing a package, run `renv::snapshot()` and commit
`renv.lock`. `renv::status()` should report a consistent state.

R packages are installed with renv, not conda: conda-forge builds can lag
behind new R releases, and installing one can downgrade R.

Paths are built with `here::here()`, anchored by the empty `.here` file at
the root. Scripts contain no absolute paths and never call `setwd()`.

## Layout

```
config/pipeline_config.yaml   paths and analysis parameters
R/scripts/                    pipeline scripts, NN_verb_object.R
R/utils/                      helper functions sourced by the scripts
reports-src/                  Quarto sources and references
docs/                         this file, audits and their check scripts
data/raw/                     data as received; read-only (not tracked)
data/derivatives/             data derived by the pipeline (not tracked)
models/, outputs/, logs/      results, figures, tables, logs (not tracked)
```

Untracked folders are created by the code on first write. Original data
releases are kept compressed in `data/archive/` as the copy to restore from.
Results are shared outside the approved team only as aggregates, after
checking them against the data use agreement; this is why `outputs/` is not
tracked.

## Configuration

Every file path and analysis parameter lives in
`config/pipeline_config.yaml`, read through `R/utils/config.R`
(`get_data_path()`, `get_parameter()`, `get_config()`). Dated file names of
data releases appear there and nowhere else, so a new release changes one
line. Alternative analysis specifications are switches in the config rather
than code edits, which keeps earlier results reproducible.

Scripts skip work whose output already exists. To re-run one, set its flag
under `scripts.force_regenerate` to `true`, run it, and set it back. Outputs
are not deleted to force a re-run, because reports read them.

## Code style

Code is checked with `lintr` (`lintr::lint_dir("R")`), configured in
`.lintr`. Lines are at most 80 characters.

Names are `snake_case` with a suffix giving the object's type: `.dt`
data.table, `.df` data.frame, `.v` vector, `.lst` list, `.fn` function,
`.fit` fitted model, `.mod` model specification, `.res` results, `.tbl`
table, `.plt` plot, `.mat` matrix, `.fct` factor, `.path` file path,
`.subdt` subset of a data.table. Constants are UPPERCASE. A leading dot marks
internal module state such as `.config`.

Data are handled with data.table. Columns are added with `:=` on each table
explicitly, and function arguments never share a name with a column used
inside `[`, where the column would silently take precedence.

Scripts are idempotent: run twice, they give the same result, because
derived values are always computed from the raw columns.

Helpers in `R/utils/` are plain files loaded with `source()`, each sourcing
its own dependencies. They are documented with roxygen-style `#'` comments.
`logging.R`, `config.R`, `data_io.R`, `validation.R`, `tables.R` (gt) and
`plotting.R` (ggplot2) are copied from the project template;
project-specific functions go at the end of `tables.R` and `plotting.R`, or
in their own file.

## Pipeline scripts

Scripts run from the project root in numeric order
(`Rscript R/scripts/01_load_data.R`). Each begins with a header stating its
purpose, method, references, inputs and outputs, then loads the config, sets
the seed from it and starts a log. Each saves its results as one named list
holding the results, sample sizes, the config values used and a timestamp,
so a result file describes itself.

## Reports

The last pipeline script gathers every number the text needs into
`outputs/<document>_env.rds`. The Quarto document loads that file and does no
analysis of its own; numbers in the text come from inline R. After a change
upstream, that script is re-run before rendering. Documents render to PDF
with xelatex.

Captions start with a short bold title ending in a full stop, followed by
the details. Captions that contain numbers are built with
`fig-cap: !expr` / `tbl-cap: !expr` from the same environment. A line of
prose never starts with inline R that prints a number, which pandoc would
read as a list item. Each document starts with
`here::i_am("reports-src/<file>.qmd")`.

Colours, type, figures, tables and slides follow `docs/STYLE.md`: McGill
red with Douglas and StoP-AD colours for the frame of the slides, and
colourblind-safe colours for data, never mixed. Every slide is checked from
a screenshot before a deck is shared.

## Provenance

The random seed is set from the config at the top of every script, and every
run writes a log to `logs/`. Before a re-run that changes results, the
current `outputs/`, `models/results/` and `data/derivatives/` are copied to
`archive/<date>_<label>/` with a short `INDEX.md` describing the snapshot.

Investigations are written up in `docs/<topic>_audit.md`, alongside a
`docs/<topic>_check.R` that reproduces every number in the write-up.

Derived variables supplied with a data release (scores, composites) are
recomputed from their inputs where possible, and the code checks that the
two agree.

## Git

Commit messages are a single imperative line. Each commit holds one logical
change, staged by file name. Corrections go on `fix/<topic>` branches.
