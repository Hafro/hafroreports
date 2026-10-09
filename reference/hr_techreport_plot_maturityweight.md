# Plot maturity as a function of stock weight

Plots observed proportion mature against stock weight (on a log scale),
coloured by time period, with a fitted model overlay. Two model types
are available: a single curve fitted to data from 2013 onwards
(`"pred"`), or period-specific curves (`"period"`).

## Usage

``` r
hr_techreport_plot_maturityweight(input_data, mat_model = c("pred", "period"))
```

## Arguments

- input_data:

  A data frame with columns `year`, `age`, `stock_weight` (grams), and
  `maturity` (proportion), as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md).

- mat_model:

  Character. Maturity model type: `"pred"` (default) fits a single GLM
  to the most recent period; `"period"` fits separate curves for three
  historical time periods.

## Value

A `ggplot2` plot object.
