# Build SAM autumn (SMH) survey index matrix

Extracts the autumn groundfish survey (SMH) abundance indices from
`model_dat`. Only data from `first_year` onwards are included, and the
final year is always excluded. Negative values are replaced with `NA`
and indices are scaled by 1000. The timing attribute is set to 0.75–0.80
of the year.

## Usage

``` r
hr_sam_smh(
  model_dat,
  minage,
  maxage,
  first_year = 1995,
  max_age = NULL,
  exclude_years = NULL
)
```

## Arguments

- model_dat:

  A data frame with columns `year`, `age`, and `smh`.

- minage:

  Minimum age to include.

- maxage:

  Maximum age to include.

- first_year:

  First survey year. Default `1995`.

- max_age:

  Ages above this are set to `NA`. Default `NULL` (all ages).

- exclude_years:

  Years to blank, e.g. `2011` (the haddock assessment excludes it for
  data quality reasons). Default `NULL`.

## Value

A numeric matrix with years as rows and ages as columns, with a `time`
attribute set to `c(0.75, 0.80)`.
