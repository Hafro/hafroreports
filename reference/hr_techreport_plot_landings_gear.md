# Plot Icelandic landings by gear (total and stacked)

Queries the `landings` table of a pax database, groups by gear (BMT,
DSE, LLN, Other), and creates a two-panel plot showing total Icelandic
landings (in thousands of tonnes) and their proportional composition by
gear over time.

## Usage

``` r
hr_techreport_plot_landings_gear(
  pcon,
  year_start = 1000,
  year_end = 9999,
  gear_group = NULL
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

## Value

A `ggplot2` / `patchwork` plot object.
