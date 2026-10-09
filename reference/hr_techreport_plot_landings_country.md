# Plot landings by country

Queries the `landings` table and creates a stacked bar chart of annual
landings in thousands of tonnes, split between Icelandic and foreign
catches. Country labels are localised to the current language.

## Usage

``` r
hr_techreport_plot_landings_country(
  pcon,
  ylab = hr_label("landings_kt"),
  xlab = hr_label("year"),
  breaks = seq(0, 1e+05, by = 10),
  year_start = 1000,
  year_end = 9999
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- ylab:

  Character. Y-axis label. Defaults to the localised label for landings
  in thousands of tonnes.

- xlab:

  Character. X-axis label. Defaults to the localised label for year.

- breaks:

  Numeric vector of x-axis break points. Default is every 10 units from
  0 to 100 000.

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`.

## Value

A `ggplot2` plot object.
