# Plot survey length distribution by year

Queries the `station` and `ldist` tables for the specified sampling type
and renders a length-frequency plot with year on the y-axis.

## Usage

``` r
hr_techreport_plot_survey_ldist(pcon, sampling_type)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- sampling_type:

  Integer or integer vector of sampling type codes (e.g. `30` for the
  spring survey, `35` for the autumn survey).

## Value

A `ggplot2` plot object.
