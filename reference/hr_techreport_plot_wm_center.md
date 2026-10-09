# Plot catch weight vs. stock weight relationship (centre panel)

Plots catch mean weight against stock mean weight on linear scales with
a linear model smoother, coloured by time period. Used to visualise the
systematic relationship between the two weight series.

## Usage

``` r
hr_techreport_plot_wm_center(input_data, assessment_year)
```

## Arguments

- input_data:

  A data frame with columns `year`, `age`, `stock_weight` (grams), and
  `catch_weight` (grams), as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md).

- assessment_year:

  Year report is providing an assessment for

## Value

A `ggplot2` plot object.
