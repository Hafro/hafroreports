# Plot a survey biomass index for advice sheet

Line plot with confidence band of a survey biomass index in the current
assessment (thousand tonnes), with a dashed line for `I_trigger`. For
index-based (category 3) stocks, whose assessment history holds the
survey index as `SSB` (as the tidypax-based advice sheets did).

## Usage

``` r
hr_advice_plot_index(
  data_assessment,
  assessment_year,
  ref_points = NULL,
  key = "SSB",
  title = NULL,
  index_ab = FALSE,
  index_ab_span = c("years", "periods"),
  index_ab_colour = "red3"
)
```

## Arguments

- data_assessment:

  Long-format assessment data as returned by
  [`hr_advice_data_assessment`](https://hafro.github.io/hafroreports/reference/hr_advice_data_assessment.md),
  index in tonnes.

- assessment_year:

  Integer. The assessment year to plot.

- ref_points:

  Named list of reference points with `I_trigger` in tonnes, or `NULL`
  for no line.

- key:

  Key of the index in `data_assessment`. Default `"SSB"`.

- title:

  Plot title. Default: biomass index.

- index_ab:

  If `TRUE`, draw the mean index of the last two years to
  `assessment_year` (index A) and of the three years before (index B) as
  red lines, as in the rfb rule (r = A / B). Default `FALSE`.

- index_ab_span:

  How the index A and B lines span the years: `"years"` (default) from
  the first to the last year of each period; `"periods"` to the half
  years between the periods, so the lines meet (index B from
  `assessment_year - 4.5` to `assessment_year - 1.5`, index A from there
  to `assessment_year`), as the old tidypax `three_over_two()` figures.

- index_ab_colour:

  Colour of the index A and B lines. Default `"red3"`.

## Value

A `ggplot2` / `ggiraph` plot object.
