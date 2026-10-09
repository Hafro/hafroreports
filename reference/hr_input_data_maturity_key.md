# Estimate maturity-at-length key from survey data

Builds a maturity ogive by fitting a quasi-binomial GLM to maturity
observations from the `measurement` table, grouped by length class (and
region, and optionally year). Observed proportions mature (by year,
length group, age, and region) are combined with model-predicted
proportions in the output. Predictions are marked by `age = NA`, and
have `year = NA` unless `by_year = TRUE`, so downstream code can
distinguish measurements from estimates.

## Usage

``` r
hr_input_data_maturity_key(
  pcon,
  lgroups = seq(0, 200, 5),
  regions = NULL,
  ignore_years = c(),
  sampling_type = 30,
  by_year = FALSE,
  sex = NULL,
  mature_above = NULL,
  immature_below = NULL,
  predict_lgroups = lgroups[lgroups > 0],
  predict_years = NULL,
  copy_years = NULL,
  aged_only = TRUE,
  tow_number = NULL,
  gear_id_filter = NULL
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- lgroups:

  Numeric vector of length group break points (lower bounds). Default is
  `seq(0, 200, 5)`.

- regions:

  Named list mapping region labels to integer MFDB area codes. If
  `NULL`, all stations are treated as one region (`"all"`). Default is
  `NULL`.

- ignore_years:

  Integer vector of years to exclude from model fitting. Default is
  [`c()`](https://rdrr.io/r/base/c.html) (no years excluded).

- sampling_type:

  Integer vector of sampling type codes to include. Default is `30`.

- by_year:

  Logical. If `TRUE`, the model includes year as a factor
  (`mat_p ~ log(lgroup) + as.factor(year)`) and predictions are made per
  year. Otherwise the model is `mat_p ~ log(lgroup) * region`
  (`mat_p ~ log(lgroup)` with a single region). Default is `FALSE`.

- sex:

  Integer sex code to restrict the observations to (e.g. `2` for
  females), or `NULL` for all fish. Default is `NULL`.

- mature_above, immature_below:

  Length (cm). Before fitting, proportions mature in length groups above
  `mature_above` are set to 1 and below `immature_below` to 0. Default
  `NULL` (not used).

- predict_lgroups:

  Length groups to predict for. Default is all `lgroups` above 0.

- predict_years:

  Years to predict for when `by_year = TRUE`. Default `NULL` is all
  years in the data; years without data are dropped.

- copy_years:

  Named vector to fill years without data from another year's
  predictions, e.g. `c("1985" = 1987, "1986" = 1987)`, or a named list
  to fill them with the mean of several years' predictions, e.g.
  `list("1985" = 2000:2002)`. Only with `by_year = TRUE`. Default
  `NULL`.

- aged_only:

  If `TRUE` (default), the model is fitted to aged fish only. `FALSE`
  also uses otoliths with a maturity stage but no age. The measured
  maturity at age is from aged fish either way.

- tow_number, gear_id_filter:

  Tow numbers (missing counts as 0) and gear ids of the stations to use,
  e.g. the index stations of the survey. Default `NULL`, all.

## Value

A tibble with columns `year`, `lgroup`, `age`, `region`, and `mat_p`.
Rows with `age = NA` are model predictions.
