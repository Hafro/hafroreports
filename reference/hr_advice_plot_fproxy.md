# Plot the fishing pressure proxy for a category 3 advice sheet

[`hr_advice_plot_fpl`](https://hafro.github.io/hafroreports/reference/hr_advice_plot_fpl.md)
for the length-based fishing pressure proxy of the rfb rule (L_F=M /
L_mean, held as `"F"` in the assessment history), with the F_MSY proxy
line and the title `hr_label("fproxy")`. The proxy is close to 1, so the
y axis can be set to a range around it instead of starting at 0.

## Usage

``` r
hr_advice_plot_fproxy(
  data_assessment,
  assessment_year,
  ref_points,
  year_start = -Inf,
  year_end = Inf,
  y_limits = NULL,
  y_pad = 0,
  points = FALSE
)
```

## Arguments

- data_assessment:

  Long-format assessment data as returned by
  [`hr_advice_data_assessment`](https://hafro.github.io/hafroreports/reference/hr_advice_data_assessment.md).

- assessment_year:

  Integer. The assessment year to plot.

- ref_points:

  Named list of reference points with `F_msy_proxy` (the only line
  drawn).

- year_start, year_end:

  First and last year of the series shown.

- y_limits:

  `NULL` (default) for the standard axis from 0; otherwise
  `c(lower, upper)`, the least range of the y axis (either may be `NA`):
  the axis is widened to the values and the F_MSY proxy, rounded out to
  0.1, with breaks every 0.1.

- y_pad:

  Also widen the axis by this much below and above the values and the
  F_MSY proxy (rounded out to 0.1), e.g. 0.1. Default 0; with
  `y_limits = NULL` and `y_pad > 0` the axis is from the data.

- points:

  Also draw the yearly values as points, so single years of a series
  with gaps show: `FALSE` (default), `TRUE` (size 0.8) or the point
  size.

## Value

A `ggplot2` / `ggiraph` plot object.
