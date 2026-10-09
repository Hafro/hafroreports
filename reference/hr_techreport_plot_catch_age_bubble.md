# Plot catch-at-age bar chart coloured by year class

Creates a stacked bar chart showing catch numbers at each age over time,
with bars coloured by year class. Ages are displayed in descending order
as facet strips on the right-hand side.

## Usage

``` r
hr_techreport_plot_catch_age_bubble(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
)
```

## Arguments

- input_data:

  A data frame with columns `year`, `age`, and `catch` (catch numbers at
  age), as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md)
  or similar.

- year_start:

  Integer. First year to display. Default is `1970`.

- year_end:

  Integer. Last year to display. Default is `9999` (no upper limit).

- age_start:

  Integer. Minimum age to display. Default is `0`.

- age_end:

  Integer. Maximum age to display. Default is `9999`.

## Value

A `ggplot2` plot object.
