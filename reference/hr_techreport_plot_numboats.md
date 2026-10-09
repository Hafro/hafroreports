# Plot number of vessels accounting for 95% of catch

Queries the `landings` table and computes the minimum number of vessels
responsible for 95\\ two-panel `patchwork` plot: a time series (left)
and a catch-vs-number-of-vessels phase plot (right).

## Usage

``` r
hr_techreport_plot_numboats(
  pcon,
  year_start = 1000,
  year_end = 9999,
  landings_tbl = dplyr::tbl(pcon, "landings"),
  ices_area_like = NULL,
  country = NULL
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

- landings_tbl:

  The landings to use, e.g. without released fish. Default the
  `landings` table of `pcon`.

- ices_area_like:

  SQL LIKE pattern of the ICES areas of the landings, e.g. `"5a%"`.
  Default `NULL`, all areas.

- country:

  Countries of the landings, e.g. `"Iceland"`. Default `NULL`, all.

## Value

A `patchwork` plot object.
