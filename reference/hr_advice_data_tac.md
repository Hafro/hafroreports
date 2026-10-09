# Assemble TAC and landings history for advice sheet

Joins historical advice, TAC, and landings data into a single table
suitable for displaying in the TAC history section of an advice sheet.
Landings are split into Icelandic and foreign components using the
`country` column.

## Usage

``` r
hr_advice_data_tac(
  advice_hist,
  tac_hist,
  landings_by_fishing_year_country,
  advice_col = "advice",
  landings_year_end = NULL
)
```

## Arguments

- advice_hist:

  A data frame with columns `assessment_year` and `advice` (recommended
  catch in tonnes) and `advice_period` (fishing year label).

- tac_hist:

  A data frame with columns `assessment_year` and `tac` (national TAC in
  tonnes).

- landings_by_fishing_year_country:

  A data frame with columns `fishing_year`, `country`, and `catch` (in
  kg).

- advice_col:

  Column of `advice_hist` shown as the advice, or the stem of a column
  per language, e.g. `"advice_basis"` for text advice in
  `advice_basis.en` / `advice_basis.is` ("No targeted fisheries").
  Default `"advice"`.

- landings_year_end:

  Last fishing year with landings in the table, by the year it starts
  in, or `NULL` (default) for all. Landings of later fishing years are
  left out (`NA`), e.g. `assessment_year - 2` for only the fishing years
  that had ended when the advice was given, as the old `the_numbers.R`
  (`filter(substr(timabil, 1, 4) != tyr - 1)`).

## Value

A tibble with columns `advice_period`, `advice`, `tac`, `icelandic`
(Icelandic catch in thousands of tonnes), `foreign`, and `total`.
