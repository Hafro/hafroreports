# Format length and otolith sampling summary table

Queries the `sampling` and `measurement` tables of a pax database and
produces a GT table summarising the number of samples, length
measurements, and/or otolith readings per gear and year. Column headers
are localised to the current language.

## Usage

``` r
hr_techreport_table_lengthsamples(
  pcon,
  mfdb_gear_code = c("BMT", "LLN", "DSE"),
  sampling_type = c(1, 2, 3, 4, 8),
  year_start = 1000,
  year_end = 9999,
  include_cols = c("lengths", "otol")
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- mfdb_gear_code:

  Character vector of MFDB gear codes to include. Default is
  `c("BMT", "LLN", "DSE")`.

- sampling_type:

  Integer vector of sampling type codes. Default is `c(1, 2, 3, 4, 8)`.

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`.

- include_cols:

  Character vector specifying which sample count columns to include.
  Valid values are `"lengths"` and `"otol"`. Default is
  `c("lengths", "otol")`.

## Value

A `gt` table object.
