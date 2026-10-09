# Map survey station locations with catch density

Creates a map of survey station locations (crosses) with the catch
density (kg/nm, proportional circles) for the spring (SMB) and autumn
(SMH) groundfish surveys in the assessment year. Zero-catch stations are
shown as grey crosses; non-zero stations have red circles proportional
to biomass per nautical mile of tow.

## Usage

``` r
hr_techreport_plot_survey_location(pcon, assessment_year)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- assessment_year:

  Integer. The SMB data from this year and the SMH data from the
  previous year are displayed.

## Value

A `ggplot2` plot object with two facets (SMB and SMH).
