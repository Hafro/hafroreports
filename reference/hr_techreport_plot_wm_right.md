# Plot year-effect estimates from stock weight growth model (right panel)

Fits the linear weight-growth model and extracts the year-effect
coefficients (\\\delta_y\\). Plots these year effects with 95\\
confidence intervals and a horizontal reference line at 0.9.

## Usage

``` r
hr_techreport_plot_wm_right(input_data, assessment_year)
```

## Arguments

- input_data:

  A data frame with columns `year`, `age`, and `stock_weight` (grams),
  as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md).

- assessment_year:

  Year report is providing an assessment for

## Value

A `ggplot2` plot object.
