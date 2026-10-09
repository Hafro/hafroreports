# Build SAM natural mortality matrix

Extracts the natural mortality at age from `model_dat`. Ages 1 through
`maxage` are included. Missing values are filled with 0.2.

## Usage

``` r
hr_sam_nm(model_dat, minage, maxage)
```

## Arguments

- model_dat:

  A data frame with columns `year`, `age`, and `M`.

- minage:

  Minimum age (currently unused; ages always start at 1).

- maxage:

  Maximum age to include.

## Value

A numeric matrix with years as rows and ages as columns.
