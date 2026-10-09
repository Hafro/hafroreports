# Plot spawning stock biomass for advice sheet

Line plot with confidence bands of SSB and, if the stock has one, the
reference biomass in the current assessment (thousand tonnes), with
lines for MGT Btrigger and Blim. Series without values (e.g. the
reference biomass of a stock with F-based advice) are left out.

## Usage

``` r
hr_advice_plot_ssb(
  data_assessment,
  assessment_year,
  ref_points,
  refbio_label = NULL,
  biomass_scale = 1000,
  y_label = NULL
)
```

## Arguments

- data_assessment:

  Long-format assessment data as returned by
  [`hr_advice_data_assessment`](https://hafro.github.io/hafroreports/reference/hr_advice_data_assessment.md).

- assessment_year:

  Integer. The assessment year to plot.

- ref_points:

  Named list of reference points (biomass in thousand tonnes):
  `MGT_btrigger` (or `MSY_btrigger` if there is no management plan),
  `B_lim`, `B_pa`.

- refbio_label:

  Text added to the reference biomass legend entry, e.g. `"(B4+)"`.
  Default `NULL`.

- biomass_scale:

  Divisor of the biomass (and its reference points' units): `1000`
  (default) for biomass in tonnes shown in thousand tonnes, `1` for
  relative biomass (B/B_MSY) shown as it is.

- y_label:

  Y axis label. Default thousand tonnes, or B/B_MSY when
  `biomass_scale = 1`.

## Value

A `ggplot2` / `ggiraph` plot object.
