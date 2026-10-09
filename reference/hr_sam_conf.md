# Build SAM configuration for Icelandic haddock (deprecated)

Deprecated: a SAM configuration is stock-specific and belongs in the
stock's repository (e.g. `had_sam_conf()` in 02-had-targets). Kept for
backward compatibility.

## Usage

``` r
hr_sam_conf(dat)
```

## Arguments

- dat:

  A SAM data object as returned by
  [`hr_sam_dat`](https://hafro.github.io/hafroreports/reference/hr_sam_dat.md).

## Value

A SAM configuration list suitable for passing to `sam.fit`.

## Details

Creates a `stockassessment` configuration object for the Icelandic
haddock stock assessment. Starts from the default configuration returned
by `defcon` and applies haddock-specific settings for age-structured
fishing mortality, observation variance grouping, and AR correlation
structures for the two survey fleets.
