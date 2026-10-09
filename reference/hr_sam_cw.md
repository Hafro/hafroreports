# Build SAM catch mean weight matrix

Extracts the catch mean weight-at-age matrix from `model_dat`. Weights
are converted from grams to kilograms. The final year row is filled with
the penultimate year's values, and missing weights are set to zero.

## Usage

``` r
hr_sam_cw(model_dat, minage, maxage)
```

## Arguments

- model_dat:

  A data frame with columns `year`, `age`, `catch`, and `catch_weight`.

- minage:

  Minimum age to include.

- maxage:

  Maximum age to include.

## Value

A numeric matrix (kg) with years as rows and ages as columns.
