# Plot management history (advice, TAC, and landings)

Combines historical advice, TAC, and total landings into a single line
plot by fishing year. User-supplied annotations (e.g. regulation
changes) can be added via `m_labels`.

## Usage

``` r
hr_techreport_plot_management(
  advice_hist,
  tac_hist,
  landings_by_fishing_year_country,
  m_labels
)
```

## Arguments

- advice_hist:

  A data frame with columns `assessment_year`, `advice`, and
  `advice_period`.

- tac_hist:

  A data frame with columns `assessment_year` and `tac`.

- landings_by_fishing_year_country:

  A data frame with columns `fishing_year`, `country`, and `catch` (in
  tonnes).

- m_labels:

  A data frame with columns `Year`, `value` (in tonnes), and `type`
  (label text) for annotations placed on the plot.

## Value

A `ggplot2` plot object.
