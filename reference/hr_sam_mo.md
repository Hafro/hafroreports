# Build SAM proportion mature matrix

Extracts the proportion mature at age from `model_dat`. Missing maturity
values are filled with 1 (fully mature).

## Usage

``` r
hr_sam_mo(model_dat, minage, maxage, na.fill = 1)
```

## Arguments

- model_dat:

  A data frame with columns `year`, `age`, and `maturity`.

- minage:

  Minimum age to include.

- maxage:

  Maximum age to include.

- na.fill:

  Value for missing maturity. Default `1`.

## Value

A numeric matrix with years as rows and ages as columns.
