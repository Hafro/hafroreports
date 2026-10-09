# Map biological sampling locations overlaid on catch density

Creates a map showing where biological samples were taken (crosses) on
top of the spatial catch density (t/nm²) from logbook data, for the
specified year range and gear types.

## Usage

``` r
hr_techreport_plot_sampling_position(
  pcon,
  mfdb_gear_codes = c("LLN", "DSE", "BMT"),
  year_start = 2000,
  year_end = 2000
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- mfdb_gear_codes:

  Character vector of MFDB gear codes to include. Default is
  `c("LLN", "DSE", "BMT")`.

- year_start:

  Integer. First year to include. Default is `2000`.

- year_end:

  Integer. Last year to include. Default is `2000`.

## Value

A `ggplot2` plot object.
