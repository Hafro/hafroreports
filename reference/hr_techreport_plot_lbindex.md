# Plot length-based survey biomass index

Computes survey biomass (or abundance) indices for a specified length
range from both the spring (SMB) and autumn (SMH) groundfish surveys,
and overlays them on a single plot. The spring survey is shown as a
ribbon with a line; the autumn survey is shown as point ranges.

## Usage

``` r
hr_techreport_plot_lbindex(
  pcon,
  length_range,
  var = "si_biomass",
  strata_stations = NULL
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- length_range:

  Integer vector of length `2` giving the minimum and maximum length
  (in cm) to include in the index.

- var:

  Character. Name of the survey index variable to plot. Default is
  `"si_biomass"`; `"si_abund"` can also be used.

- strata_stations:

  Fixed station list (columns `sampling_type`, `station` and `stratum`),
  e.g.
  `dplyr::tbl(pcon, "strata_stations") |> dplyr::filter(stratification == "new_strata")`.
  If given, stations get their stratum from it as in the survey indices,
  not from the tow position. Default `NULL`, strata from tow positions.

## Value

A `ggplot2` plot object.
