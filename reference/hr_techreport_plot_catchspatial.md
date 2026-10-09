# Plot spatial distribution of catch

Creates a map of catch per unit tow area (t/nm²) for the selected years,
overlaid on ocean depth contours. Each grid cell is rounded to one
decimal degree of latitude and longitude. If more than one year is
supplied, years are shown as facets.

## Usage

``` r
hr_techreport_plot_catchspatial(
  pcon,
  years,
  low_res = TRUE,
  breaks = c(0, 1, 2, seq(3, 20, by = 3), 40, 60),
  na.fill = -50
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- years:

  Integer vector of years to map.

- low_res:

  Logical. If `TRUE` (the default), uses a lower resolution base map
  (faster to render).

- breaks:

  Colour breaks of the catch (t/nm²). The default (0 to 60 t/nm²) suits
  the large stocks; a stock with small catches per area needs finer
  breaks, e.g. `seq(0, 0.5, by = 0.1)`.

- na.fill:

  Value given to cells without catch, see
  [`pax::pax_map_layer_catch()`](https://rdrr.io/pkg/pax/man/pax_map.html).
  Default `-50`.

## Value

A `ggplot2` plot object.
