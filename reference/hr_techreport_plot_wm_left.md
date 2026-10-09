# Plot stock weight growth model (left panel)

Plots weight at age \\a+1\\ in year \\y+1\\ against weight at age \\a\\
in year \\y\\ on log scales, with a linear model smoother. Colours
distinguish the current assessment year from historical data.

## Usage

``` r
hr_techreport_plot_wm_left(input_data, assessment_year)
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
