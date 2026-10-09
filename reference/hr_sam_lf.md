# Build SAM landing fraction matrix

Creates a matrix of landing fractions (proportion of catch that is
landed, i.e. not discarded) with the same dimensions and dimnames as the
catch-at-age matrix. All values are set to 1, indicating no discarding.

## Usage

``` r
hr_sam_lf(model_dat, cn)
```

## Arguments

- model_dat:

  Unused; retained for consistency with other `hr_sam_*` helpers.

- cn:

  The catch-at-age matrix returned by
  [`hr_sam_cn`](https://hafro.github.io/hafroreports/reference/hr_sam_cn.md),
  used to determine the required dimensions.

## Value

A numeric matrix of ones with the same shape as `cn`.
