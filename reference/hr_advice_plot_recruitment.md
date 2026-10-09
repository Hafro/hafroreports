# Plot recruitment for advice sheet

Bar chart of recruitment (millions) with confidence intervals for the
current assessment.

## Usage

``` r
hr_advice_plot_recruitment(
  data_assessment,
  assessment_year,
  recruitment_age = NULL,
  title = NULL,
  scale = 1000,
  y_label = NULL
)
```

## Arguments

- data_assessment:

  Long-format assessment data as returned by
  [`hr_advice_data_assessment`](https://hafro.github.io/hafroreports/reference/hr_advice_data_assessment.md).

- assessment_year:

  Integer. The assessment year to plot.

- recruitment_age:

  Recruitment age, shown in the title, or `NULL` for no age. Default
  `NULL`.

- title:

  Plot title, e.g. `hr_label("juvenile_index", bold = TRUE)` for a
  juvenile survey index. Default: recruitment (at age).

- scale:

  Divisor of the recruitment: `1e3` (default) for thousands shown in
  millions; e.g. `1` for an index shown as it is.

- y_label:

  Y axis label. Default millions, or none when `scale` is not `1e3`.

## Value

A `ggplot2` / `ggiraph` plot object.
