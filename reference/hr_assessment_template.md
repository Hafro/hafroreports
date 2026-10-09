# Create an empty assessment data template

Returns a one-row tibble with all `NA` values and the column structure
expected by the assessment data functions in this package. Useful as a
starting point when building assessment data frames manually.

## Usage

``` r
hr_assessment_template()
```

## Value

A tibble with columns `year`, `species`, `median_SSB`, `low_SSB`,
`high_SSB`, `median_F`, `low_F`, `high_F`, `median_recruitment`,
`low_recruitment`, `high_recruitment`, `landings`, `median_refbio`,
`low_refbio`, `high_refbio`, `median_HR`, `low_HR`, `high_HR`, and
`assessment_year`, all set to `NA`.
