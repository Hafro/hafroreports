# Fetch assessment results from ICES SAG

Downloads the summary table and custom columns for a given stock from
the ICES Stock Assessment Graphs (SAG) database and reshapes them into
the standard assessment data frame used by this package.

## Usage

``` r
hr_assessment_from_sag(
  assessment_year,
  species,
  ices_stock_key_label,
  ices_median_refbio = NULL
)
```

## Arguments

- assessment_year:

  Numeric. The assessment year to retrieve.

- species:

  Character. Species identifier to attach to the output (not used for
  filtering; purely informational).

- ices_stock_key_label:

  Character. The ICES stock key label (e.g. `"had.27.5a"`).

- ices_median_refbio:

  Character or `NULL`. Name of the custom SAG column to use as the
  reference biomass median. If `NULL` no reference biomass column is
  included.

## Value

A tibble with columns `year`, `species`, `assessment_year`,
`low_recruitment`, `median_recruitment`, `high_recruitment`, `low_SSB`,
`median_SSB`, `high_SSB`, `median_refbio`, `landings`, `low_HR`,
`median_HR`, and `high_HR`. Harvest rate estimates for the assessment
year itself are set to `NA`.
