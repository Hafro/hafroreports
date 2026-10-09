# Build SAM stock mean weight matrix

Extracts the stock mean weight-at-age matrix from `model_dat`. Weights
are converted from grams to kilograms. Missing weights are filled with a
small non-zero value (0.001 kg).

## Usage

``` r
hr_sam_sw(model_dat, minage, maxage)
```

## Arguments

- model_dat:

  A data frame with columns `year`, `age`, and `stock_weight`.

- minage:

  Minimum age to include.

- maxage:

  Maximum age to include.

## Value

A numeric matrix (kg) with years as rows and ages as columns.
