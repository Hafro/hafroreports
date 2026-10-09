# Plot catch proportions by year class (total and stacked)

Creates a two-panel plot showing total catch weight (in thousands of
tonnes) and its proportional composition by year class over time.

## Usage

``` r
hr_techreport_plot_catch_age_prop(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
)
```

## Arguments

- input_data:

  A data frame with columns `year`, `age`, `catch` (numbers), and
  `catch_weight` (mean weight in grams), as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md)
  or similar.

- year_start:

  Integer. First year to display. Default is `1970`.

- year_end:

  Integer. Last year to display. Default is `9999`.

- age_start:

  Integer. Minimum age to include. Default is `0`.

- age_end:

  Integer. Maximum age to include. Default is `9999`.

## Value

A `ggplot2` / `patchwork` plot object.
