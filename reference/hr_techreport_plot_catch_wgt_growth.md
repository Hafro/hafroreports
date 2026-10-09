# Plot catch weight growth by age and year class

Creates a faceted segment plot showing the annual catch mean weight at
each age (coloured by year class) relative to the long-term mean weight
for that age. Points above the horizontal mean line indicate
above-average individual weights for that year class.

## Usage

``` r
hr_techreport_plot_catch_wgt_growth(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
)
```

## Arguments

- input_data:

  A data frame with columns `year`, `age`, `catch`, `catch_weight`
  (grams), and `stock_weight` (grams), as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md).

- year_start:

  Integer. First year to display. Default is `1970`.

- year_end:

  Integer. Last year to display. Default is `9999`.

- age_start:

  Integer. Minimum age. Default is `0`.

- age_end:

  Integer. Maximum age. Default is `9999`.

## Value

A `ggplot2` plot object.
