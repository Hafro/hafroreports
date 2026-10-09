# Plot SAM model residuals by year, age, and fleet

Creates a bubble plot of SAM model residuals from a fitted SAM object.
Bubble size represents residual magnitude and colour indicates sign (red
= negative, blue = positive). Residuals are faceted by fleet.

## Usage

``` r
hr_techreport_plot_samres_resid(res)
```

## Arguments

- res:

  A fitted SAM model object, as returned by `sam.fit`.

## Value

A `ggplot2` plot object.
