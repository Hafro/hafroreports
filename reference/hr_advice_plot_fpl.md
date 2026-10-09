# Plot fishing pressure for advice sheet

Line plot with confidence band of the harvest rate (`"HR"`) or fishing
mortality (`"F"`) in the current assessment, with dashed lines for the
management, MSY and precautionary reference points the stock has
(`HR_mgt`, `HR_msy`, `HR_pa` or `F_mgt`, `F_msy`, `F_pa` in
`ref_points`), and for category 3 stocks the MSY proxy harvest rate
(`HR_msy_proxy`).

## Usage

``` r
hr_advice_plot_fpl(
  data_assessment,
  assessment_year,
  ref_points,
  fishing_pressure = c("HR", "F"),
  title = NULL,
  show_lim = FALSE,
  points = FALSE,
  y_label = ""
)
```

## Arguments

- data_assessment:

  Long-format assessment data as returned by
  [`hr_advice_data_assessment`](https://hafro.github.io/hafroreports/reference/hr_advice_data_assessment.md).

- assessment_year:

  Integer. The assessment year to plot.

- ref_points:

  Named list of reference points.

- fishing_pressure:

  `"HR"` (harvest rate) or `"F"`. Default `"HR"`.

- title:

  Plot title. Default: harvest rate, or fishing mortality.

- show_lim:

  If `TRUE`, also draw the limit reference point (`F_lim` or `HR_lim`),
  as for relative (F/F_MSY) stocks. Default `FALSE`.

- points:

  If `TRUE`, also draw the yearly values as points, so single years of a
  series with gaps show. Default `FALSE`.

- y_label:

  Y axis label. Default none.

## Value

A `ggplot2` / `ggiraph` plot object.
