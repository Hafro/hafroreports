# Plot survey biomass by geographic region (total and stacked)

Computes survey biomass indices for both the spring (SMB) and autumn
(SMH) groundfish surveys, broken down by geographic region, and produces
a two-panel plot per survey showing total biomass and regional
proportions over time.

## Usage

``` r
hr_techreport_plot_survey_byarea(
  pcon,
  regions = list(W = 101, NW = 102, NE = c(103, 104, 105), SE = c(107, 106), SW = 108,
    Other = pax::pax_add_other()),
  strata_stations = NULL,
  keep_order = FALSE
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- regions:

  Named list mapping region labels to integer MFDB area codes. Default
  regions are W (101), NW (102), NE (103–105), SE (106–107), SW (108),
  and Other (all remaining). The names are the labels, translated by
  [`hr_label()`](https://hafro.github.io/hafroreports/reference/hr_label.md)
  if they are known keys.

- strata_stations:

  Fixed station list (columns `sampling_type`, `station` and `stratum`),
  e.g.
  `dplyr::tbl(pcon, "strata_stations") |> dplyr::filter(stratification == "new_strata")`.
  If given, stations get their stratum from it as in the survey indices,
  not from the tow position. Default `NULL`, strata from tow positions.

- keep_order:

  If `TRUE`, the regions are stacked and listed in the order of
  `regions`. Default `FALSE`, alphabetical order of the labels.

## Value

A `ggplot2` / `patchwork` plot object split by survey.
