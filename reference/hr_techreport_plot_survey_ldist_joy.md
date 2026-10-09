# Plot survey length distributions as joy plots for both surveys

Creates a side-by-side ridgeline (joy) plot of length distributions from
the spring (sampling type 30) and autumn (sampling type 35) groundfish
surveys. Each survey is shown in a separate panel.

## Usage

``` r
hr_techreport_plot_survey_ldist_joy(pcon, year_start = 1000, year_end = 9999)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`. The autumn survey is
  excluded for the final year.

## Value

A `patchwork` plot object with two panels.
