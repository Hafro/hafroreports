# Plot catch by geographic region (total and stacked)

Queries the `logbook` table of a pax database and creates a two-panel
plot showing total catch and proportional composition by region (W, NW,
NE, SE, SW, and other) over time. Region names are localised to the
current language setting.

## Usage

``` r
hr_techreport_plot_catch_region(
  pcon,
  depth_class = c(0, 100, 200, 300),
  regions = list(W = 101, NW = 102, NE = c(103, 104, 105), SE = c(107, 106), SW = 108),
  year_start = 1000,
  year_end = 9999,
  keep_order = FALSE
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- depth_class:

  Positive numeric vector of depth breaks.

- regions:

  Named list mapping region labels to integer MFDB area codes. Default
  regions are W (101), NW (102), NE (103–105), SE (106–107), SW (108).
  The names are the labels, translated by
  [`hr_label()`](https://hafro.github.io/hafroreports/reference/hr_label.md)
  if they are known keys (e.g. `"NW"` is `"NV"` in Icelandic).

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`.

- keep_order:

  If `TRUE`, the regions are stacked and listed in the order of
  `regions`, other last. Default `FALSE`, alphabetical order of the
  labels.

## Value

A `ggplot2` / `patchwork` plot object.
