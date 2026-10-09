# Assessment summary from a SAM fit

Builds the current assessment's rows of the assessment history (the
format of
[`hr_assessment_template`](https://hafro.github.io/hafroreports/reference/hr_assessment_template.md))
from a SAM fit, so the history doesn't depend on ICES SAG. Recruitment,
SSB and F (Fbar) come from the fit, the reference biomass and harvest
rate from `SAMutils::rby.sam()` if `ref_bio_type` is given. F and the
harvest rate are left empty in the assessment year (no catch data), as
are landings.

## Usage

``` r
hr_assessment_from_fit(
  sam_fit,
  input_data_landings,
  species,
  assessment_year,
  ref_bio_type = NULL
)
```

## Arguments

- sam_fit:

  A SAM fit (`sam_fit$fit` from `SAMutils::full_sam_fit()`).

- input_data_landings:

  Landings by year with columns `year` and `catch` (kg), e.g. from
  [`hr_input_data_landings`](https://hafro.github.io/hafroreports/reference/hr_input_data_landings.md).

- species:

  Species code.

- assessment_year:

  Assessment year.

- ref_bio_type:

  `NULL` (no reference biomass, e.g. for F-based advice), `"length"` or
  `"age"` (passed to `SAMutils::rby.sam()`). Default `NULL`.

## Value

A tibble with the columns of
[`hr_assessment_template`](https://hafro.github.io/hafroreports/reference/hr_assessment_template.md),
landings in tonnes.
