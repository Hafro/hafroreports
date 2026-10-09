# Plot quota transfers between years and between species

Queries the `quotatransfer` table of a pax database and produces a
faceted bar chart showing quota transferred between fishing years and
between species, both in absolute terms (thousands of tonnes) and as a
percentage of the permanent quota.

## Usage

``` r
hr_techreport_plot_quotatransfer(pcon, assessment_year)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- assessment_year:

  Integer. Only fishing years starting before `assessment_year` are
  included.

## Value

A `ggplot2` plot object with four facets.
