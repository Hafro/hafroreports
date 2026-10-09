# Format vessel and catch summary table by gear

Queries the `landings` table, groups by gear, and produces a GT table
summarising the number of vessels and catch (in thousands of tonnes) per
gear type per year. Column headers are localised to the current
language.

## Usage

``` r
hr_techreport_table_boatsummary(
  pcon,
  year_start = 1000,
  year_end = 9999,
  gear_group = NULL,
  ices_area_like = NULL
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`.

- gear_group:

  Gear groups, as for
  [`pax::pax_landings_by_gear()`](https://rdrr.io/pkg/pax/man/pax_landings.html).
  Default `NULL` uses its default groups.

- ices_area_like:

  SQL LIKE pattern of the ICES areas to include, e.g. `"5a%"`. Default
  `NULL`, all areas in the landings table. The areas are pooled: a
  vessel landing in several areas counts once.

## Value

A `gt` table object.
