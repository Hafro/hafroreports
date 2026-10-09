# The layout of a stock repository: targets and Quarto

Each MFRI stock assessment (`01-cod`, `03-sai`, `23-ple`, … `61-reb`) is
a repository with the same layout, copied from the template
`02-had-targets`. hafroreports and pax are the shared code; the
repository holds only the stock’s settings, its own functions and the
report text. This vignette describes the layout. Nothing in it runs
here: the pipelines need the MFRI database or a copy of a stock’s
`pax_db`.

## Files

    02-had-targets/
    ├── _targets.yaml              three targets projects, one store each
    ├── script_assessment_model.R  data, model, forecast, history tables
    ├── script_techreport.R        tech reports (EN, IS)
    ├── script_advice.R            advice sheets (EN, IS)
    ├── run.R                      runs the three projects in order
    ├── config.R                   species, years, reference points, table text
    ├── quarto_setup.R             sourced at the top of each .qmd
    ├── R/                         the stock's own functions (input_data.R, sam.R, hist.R, ...)
    ├── data/                      history CSVs: advice_hist, tac_hist, assessment, ...
    ├── advice_en.qmd, advice_is.qmd
    ├── techreport_en.qmd, techreport_is.qmd
    ├── theme/                     SCSS for the reports
    ├── library.bib
    └── renv.lock                  pinned package versions (pax, hafroreports, SAMutils, ...)

## Three targets projects

`_targets.yaml` defines three projects, each with its own script and
store:

``` yaml
assessment_model:
  store: _assessment_model
  script: script_assessment_model.R
techreport:
  store: _techreport
  script: script_techreport.R
advice:
  store: _advice
  script: script_advice.R
```

`run.R` runs them in order:

``` r

Sys.setenv(TAR_PROJECT = "assessment_model")
targets::tar_make()

Sys.setenv(TAR_PROJECT = "techreport")
targets::tar_make()

Sys.setenv(TAR_PROJECT = "advice")
targets::tar_make()
```

In the stock repositories (all but the template) `run.R` first calls
`check_no_mar()` (`R/check_no_mar.R`), which stops if any code outside
the `pax_db` target reads `mar` or `mfdb` directly, so every input comes
from `pax_db`.

### script_assessment_model.R

All the computation is here. Every script starts the same way:

``` r

library(pax)
library(targets)
library(tarchetypes)

tar_option_set(packages = c("hafroreports", "pax", "tidyverse"), tidy_eval = TRUE)

tar_source("config.R")
tar_source() # R/*.R
```

followed by the targets (see
[`vignette("input-data")`](https://hafro.github.io/hafroreports/articles/input-data.md)
and
[`vignette("history-tables")`](https://hafro.github.io/hafroreports/articles/history-tables.md)):

1.  `pax_db`: the local DuckDB, built with
    [`pax_from_mar()`](https://rdrr.io/pkg/pax/man/pax_from_mar.html) or
    opened from `PAX_SOURCE_DB` (`format = pax_tar_format_duckdb()`);
2.  the input data chain, ending in `input_data`;
3.  the model: `sam_dat`, `sam_conf`, `sam_fit` (or a Muppet fit,
    `01-cod`; or the rfb / chr rule for category 3 stocks,
    e.g. `rfb_prognosis` in `25-wit`);
4.  the forecast (`prognosis`, `stock_dev`);
5.  the history tables (`advice_hist`, `tac_hist`, `assessment_hist`)
    with `update_hist()`;
6.  summaries the documents need (`landings_by_gear`,
    `landings_by_fishing_year_country`).

Tables use `format = pax::pax_tar_format_parquet()`, model objects
`format = "rds"`.

To run a stock without the MFRI database, copy its `pax_db` and point to
it:

``` r

Sys.setenv(PAX_SOURCE_DB = "/path/to/pax_db.duckdb")
```

Some stocks also read data that are not in `pax_db` (Greenland landings,
foreign surveys, preliminary ICES catches). Those are targets or `data/`
files of their own and are noted in the stock’s README.

### script_techreport.R and script_advice.R

These only render documents, with
[`tarchetypes::tar_quarto()`](https://docs.ropensci.org/tarchetypes/reference/tar_quarto.html).
The documents load results from the `_assessment_model` store:

``` r

# in advice_en.qmd
targets::tar_load(c(advice_hist, tac_hist, landings_by_gear), store = "_assessment_model")
```

targets doesn’t track dependencies across stores, and it doesn’t see
code that the documents
[`source()`](https://rdrr.io/r/base/source.html). Without help, a refit
or a change in `R/` leaves the reports silently stale. So the scripts
list the files the documents depend on and pass them as `extra_files`
(`23-ple`):

``` r

# Results the documents load from the assessment_model pipeline
assessment_model_objects <- file.path(
  "_assessment_model", "objects",
  c("assessment_hist", "advice_hist", "landings_by_fishing_year_country",
    "landings_by_gear", "stock_dev", "tac_hist", "prognosis")
)
# Stock functions and settings are sourced inside the documents
document_inputs <- c(
  assessment_model_objects,
  "config.R",
  list.files("R", full.names = TRUE)
)

list(
  tar_quarto(advice_en, path = "advice_en.qmd", extra_files = document_inputs, quiet = FALSE),
  tar_quarto(advice_is, path = "advice_is.qmd", extra_files = document_inputs, quiet = FALSE)
)
```

When an object in the list changes, its file changes and the documents
rebuild; otherwise
[`tar_make()`](https://docs.ropensci.org/targets/reference/tar_make.html)
skips them. Keep the list in step with the
[`tar_load()`](https://docs.ropensci.org/targets/reference/tar_load.html)
calls in the documents. The template `02-had-targets` doesn’t have
`extra_files` yet.

## The documents

Each language has its own `.qmd` with `lang: en` or `lang: is` in the
front matter. The first chunk sources the settings and `quarto_setup.R`,
which sets the language for pax and hafroreports and loads the stock’s
functions:

``` r

# quarto_setup.R
lang <- rmarkdown::metadata$lang
options(pax.lang = lang, hr.lang = lang)

library(pax)
library(hafroreports)
targets::tar_source()
```

The rest of the document calls the `hr_advice_*()` or
`hr_techreport_*()` functions (see
[`vignette("advice-sheets")`](https://hafro.github.io/hafroreports/articles/advice-sheets.md)
and
[`vignette("techreport-figures")`](https://hafro.github.io/hafroreports/articles/techreport-figures.md))
and the stock’s own wrappers in `R/`. The English and Icelandic
documents should differ only in their text.

In chunk options, quote expressions after `!expr`:
`#| fig-cap: !expr "paste0('Catch in ', assessment_year)"`. Without the
quotes Quarto fails with “YAML tags like !expr must be followed by YAML
strings”.

## config.R

All the stock’s settings: `species`, `year_start`, `year_end`,
`assessment_year` (the year of the advice, not the last catch year),
`age_end`, `publication_date`, the reference points (biomass in thousand
tonnes, for the figures), and the text of the basis, reference point and
prognosis input tables and the TAC table footnotes, in English and
Icelandic. Keep stock settings here rather than in the scripts or the
documents.

## Packages

The repository pins its packages with renv (`renv.lock`). After cloning:

``` r

renv::restore()
```

- hafroreports’ DESCRIPTION pins pax and SAMutils to their working
  branches
  (`Remotes: github::hafro/pax@smn-survey, github::hafro/SAMutils@fit-options`).
  renv has still installed pax main from its cache at times; check with
  `exists("pax_mar_strata_stations", asNamespace("pax"))`.
- To test a development version of hafroreports in a stock repository:
  `renv::install("local::../hafroreports", dependencies = "never")`, and
  switch back to the GitHub version before `renv::snapshot()`.
- targets doesn’t track package versions. After installing a new pax,
  run `targets::tar_invalidate(pax_db)` so the database is rebuilt;
  after a new hafroreports, invalidate the targets that use the changed
  functions.
- ROracle (for
  [`pax_from_mar()`](https://rdrr.io/pkg/pax/man/pax_from_mar.html)) is
  not in the lockfile; install it separately on a machine with database
  access.

## Porting an old assessment

The older assessments (`DAG/NN-stock`, tidypax and `runall.R`) are being
moved to this layout one by one. The pitfalls found so far (raised
twice, tow filters, strata, plus groups, hard-coded haddock values,
cross-store rebuilds, published history rows) are the “things to know”
sections of the other vignettes. Check the numbers of a port against the
published assessment: TAC, SSB, reference biomass and the input data by
year and age.

## AI use

This vignette was drafted with Claude (Anthropic) in October 2026 and
has not yet been checked by a person (MFRI policy on AI use).
