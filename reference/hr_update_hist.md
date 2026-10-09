# Merge and update a historical data series

Combines one or more data frames (or CSV file paths) into a single
historical table. When a new data frame contains rows for an
`assessment_year` already present in the accumulator, the old rows are
replaced. `NULL` arguments are silently skipped. Template rows (where
`assessment_year` is `NA`) are removed from the output.

## Usage

``` r
hr_update_hist(...)
```

## Arguments

- ...:

  Data frames or character paths to CSV files, each with an
  `assessment_year` column. Later arguments take precedence over earlier
  ones for the same `assessment_year` values.

## Value

A tibble containing all rows from the merged inputs, with duplicate
`assessment_year` entries resolved in favour of the latest argument, and
template rows (`assessment_year = NA`) removed.

## Details

This is useful for maintaining a rolling series across assessment years,
e.g. storing advice or TAC history incrementally.
