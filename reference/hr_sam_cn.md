# Build SAM catch-at-age matrix

Extracts the catch-at-age matrix from `model_dat`, replacing zero and
missing catches with `NA`. The final (interim) year row is set to `NA`
to avoid using incomplete data.

## Usage

``` r
hr_sam_cn(model_dat, minage, maxage)
```

## Arguments

- model_dat:

  A data frame with columns `year`, `age`, and `catch`.

- minage:

  Minimum age to include.

- maxage:

  Maximum age to include.

## Value

A numeric matrix with years as rows and ages as columns.
