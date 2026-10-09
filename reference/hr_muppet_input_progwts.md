# Generate MUPPET projection weights file

Fits linear models for stock weight growth and the stock-to-catch weight
relationship, and a quasi-binomial GLM for maturity, then projects
weights and maturity forward for 11 years beyond `year_end`. The result
is written as `Files/ProgWts.dat` in the format expected by MUPPET.

## Usage

``` r
hr_muppet_input_progwts(assessment_input_data, year_end)
```

## Arguments

- assessment_input_data:

  A data frame of model input data as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md),
  containing columns `year`, `age`, `stock_weight`, `catch_weight`, and
  `maturity`.

- year_end:

  Integer. The assessment year; projections are produced for years
  `year_end` through `year_end + 11`.

## Value

A named list with one element: `"Files/ProgWts.dat"` mapped to a
tab-delimited character string with columns `year`, `age`,
`catch_weight`, `stock_weight`, `maturity`, and `ssbwts`.
