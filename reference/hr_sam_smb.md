# Build SAM spring (SMB) survey index matrix

Extracts the spring groundfish survey (SMB) abundance indices from
`model_dat`. Only data from 1985 onwards are used. Negative values are
replaced with `NA` and indices are scaled by 1000. The timing attribute
is set to weeks 0.15–0.20 of the year.

## Usage

``` r
hr_sam_smb(model_dat, minage, maxage)
```

## Arguments

- model_dat:

  A data frame with columns `year`, `age`, and `smb`.

- minage:

  Minimum age to include.

- maxage:

  Maximum age to include.

## Value

A numeric matrix with years as rows and ages as columns, with a `time`
attribute set to `c(0.15, 0.20)`.
