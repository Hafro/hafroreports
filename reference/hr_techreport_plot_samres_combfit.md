# Plot SAM combined survey fit

Computes observed and predicted survey biomass indices (stock-weight
expanded, in thousands of tonnes) from SAM model residuals, and plots
points (observed) with overlaid lines (predicted) for each fleet.

## Usage

``` r
hr_techreport_plot_samres_combfit(res, model_data)
```

## Arguments

- res:

  A fitted SAM model object, as returned by `sam.fit`.

- model_data:

  A data frame with columns `year`, `age`, and `stock_weight` used to
  expand numbers to biomass.

## Value

A `ggplot2` plot object.
