# Plot spring and autumn survey indices at age coloured by year class

Creates a faceted bar chart of spring (SMB) and autumn (SMH) survey
abundance indices at each age over time. Bars are coloured by year class
and ages are displayed in descending order.

## Usage

``` r
hr_techreport_plot_survey_age_bubble(
  input_data,
  year_start = 1970,
  year_end = 9999,
  age_start = 0,
  age_end = 9999
)
```

## Arguments

- input_data:

  A data frame with columns `year`, `age`, `smb` (spring survey index),
  and `smh` (autumn survey index), as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md).

- year_start:

  Integer. First year to display. Default is `1970`.

- year_end:

  Integer. Last year to display. Default is `9999`.

- age_start:

  Integer. Minimum age. Default is `0`.

- age_end:

  Integer. Maximum age. Default is `9999`.

## Value

A `ggplot2` plot object.
