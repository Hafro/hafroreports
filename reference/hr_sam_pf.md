# Build SAM proportion of F before spawning matrix

Creates a matrix with the same dimensions as the catch-at-age matrix,
filled with `value`, the proportion of fishing mortality that occurs
before spawning (e.g. 0.4 for F and 0.3 for M in the haddock
assessment).

## Usage

``` r
hr_sam_pf(model_dat, cn, value = 0)
```

## Arguments

- model_dat:

  Unused; retained for consistency with other `hr_sam_*` helpers.

- cn:

  The catch-at-age matrix returned by
  [`hr_sam_cn`](https://hafro.github.io/hafroreports/reference/hr_sam_cn.md),
  used to determine the required dimensions.

- value:

  Proportion before spawning. Default `0`.

## Value

A numeric matrix of `value` with the same shape as `cn`.
