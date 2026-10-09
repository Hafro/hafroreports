# Plot catch by depth class (total and stacked)

Queries the `logbook` table of a pax database and creates a two-panel
plot showing total catch (in thousands of tonnes) and proportional
composition by depth class (0–100 m, 100–200 m, 200–300 m, \>300 m) over
time.

## Usage

``` r
hr_techreport_plot_catchdepth(
  pcon,
  depth_class = c(0, 100, 200, 300),
  year_start = 1000,
  year_end = 9999,
  mfdb_gear_code = NULL
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- depth_class:

  Positive numeric vector of depth breaks.

- year_start:

  Integer. First year to include. Default is `1000` (no lower limit).

- year_end:

  Integer. Last year to include. Default is `9999`.

- mfdb_gear_code:

  Gear codes of the logbook records to include, e.g. `c("BMT", "DSE")`.
  Default `NULL`, all gears.

## Value

A `ggplot2` / `patchwork` plot object.
