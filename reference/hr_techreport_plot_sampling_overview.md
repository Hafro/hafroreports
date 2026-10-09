# Plot sampling coverage by month, gear, and sampling type

Creates a faceted tile/bar plot comparing the monthly distribution of
biological samples to the monthly distribution of landings, grouped by
gear and year. Sampling type is shown as stacked bar fill; the number of
samples per month is annotated above each bar.

## Usage

``` r
hr_techreport_plot_sampling_overview(
  pcon,
  mfdb_gear_codes = c("LLN", "DSE", "BMT"),
  sampling_types = c(1, 2, 3, 4, 8),
  gear_group = list(BMT = "BMT", LLN = "LLN", DSE = "DSE"),
  year_start = 1000,
  year_end = 9999,
  landings_tbl = dplyr::tbl(pcon, "landings"),
  ices_area_like = NULL,
  country = NULL
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- mfdb_gear_codes:

  Character vector of MFDB gear codes to include. Default is
  `c("LLN", "DSE", "BMT")`.

- sampling_types:

  Integer vector of sampling type codes. Default is `c(1, 2, 3, 4, 8)`.

- gear_group:

  Named list mapping gear group labels to MFDB gear codes, used to group
  stations and landings by gear. Default groups are `BMT`, `LLN`, and
  `DSE`.

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`.

- landings_tbl:

  The landings to use, e.g. without released fish. Default the
  `landings` table of `pcon`.

- ices_area_like:

  SQL LIKE pattern of the ICES areas of the landings, e.g. `"5a%"`.
  Default `NULL`, all areas.

- country:

  Countries of the landings, e.g. `"Iceland"`. Default `NULL`, all.

## Value

A `ggplot2` plot object.
