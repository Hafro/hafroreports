# Format forecast assumptions table for advice sheet

Renders a formatted `flextable` showing the assumptions for the interim
year and forecast: variable, value and notes. Row labels come from the
`variable.en` / `variable.is` columns of `data_prog_input` if present,
otherwise they are built from `name` (`ssb`, `rec`, `catch`, `HR`,
`fbar`, `refbio`) and `year`. Rows are shown in the order of
`data_prog_input`, or of `order`.

## Usage

``` r
hr_advice_table_prog_input(
  data_prog_input,
  assessment_year,
  recruitment_age = NULL,
  order = NULL
)
```

## Arguments

- data_prog_input:

  A data frame with columns `name`, `year`, `value`, `notes.en`,
  `notes.is`, and optionally `variable.en`, `variable.is`.

- assessment_year:

  Integer. The assessment year (unused, kept for compatibility).

- recruitment_age:

  Recruitment age for the built-in recruitment label. Default `NULL` (no
  age).

- order:

  Row indices to show, in order, e.g. `c(5, 3, 2, 4, 6, 1)`. Default
  `NULL` (data order).

## Value

A `flextable` object styled for inclusion in an advice sheet.
