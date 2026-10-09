# Plot retrospective comparison of recent assessments for advice sheet

Faceted line plot comparing the current assessment (red) with the
assessments of the previous years (black) over the last 15 years:
fishing pressure (harvest rate or F), SSB, the reference biomass (if the
stock has one) and recruitment. Dashed lines show the reference points
the stock has.

## Usage

``` r
hr_advice_plot_retro(
  data_assessment,
  ref_points,
  assessment_year,
  fishing_pressure = c("HR", "F"),
  recruitment_from = NULL,
  biomass_scale = 1000,
  show_lim = FALSE,
  panel_order = NULL
)
```

## Arguments

- data_assessment:

  Long-format assessment data as returned by
  [`hr_advice_data_assessment`](https://hafro.github.io/hafroreports/reference/hr_advice_data_assessment.md),
  with several assessment years.

- ref_points:

  Named list of reference points (biomass in thousand tonnes).

- assessment_year:

  Integer. The current assessment year.

- fishing_pressure:

  `"HR"` (harvest rate) or `"F"`. Default `"HR"`.

- recruitment_from:

  First assessment year whose recruitment is shown, e.g. the year the
  recruitment age changed (earlier assessments estimated recruitment at
  another age). Default `NULL`, all assessments.

- biomass_scale:

  Divisor of the biomass and recruitment: `1000` (default) for thousand
  tonnes (millions), `1` for relative biomass (B/B_MSY) shown as it is.

- show_lim:

  If `TRUE`, also draw the limit reference point of the fishing pressure
  (`F_lim` or `HR_lim`). Default `FALSE`.

- panel_order:

  Keys in the order of the panels, e.g. `c("F", "recruitment", "SSB")`,
  the same in every language. Default `NULL`: alphabetical by label, as
  before.

## Value

A `ggplot2` / `ggiraph` plot object.
