# Plot landings by gear for advice sheet

Creates an interactive stacked bar chart showing total landings by gear
type over time, with colours and labels adjusted for the current
language setting. Each gear group keeps its colour whichever groups the
stock uses.

## Usage

``` r
hr_advice_plot_landings(
  data_landings,
  assessment_year,
  legend_position = c(0.35, 0.85),
  year_start = 1978
)
```

## Arguments

- data_landings:

  A data frame as returned by
  [`hr_advice_data_landings`](https://hafro.github.io/hafroreports/reference/hr_advice_data_landings.md).

- assessment_year:

  Integer. Used to set the x-axis upper limit.

- legend_position:

  Legend position inside the panel. Default `c(0.35, 0.85)`.

- year_start:

  First year on the x axis. Default 1978.

## Value

A `ggplot2` / `ggiraph` plot object.
