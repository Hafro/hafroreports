# Plot commercial length-frequency distribution as a joy plot

Queries the `station` table of a pax database, filters by gear code and
sampling type, and produces a ridgeline (joy) plot of length
distributions by year.

## Usage

``` r
hr_techreport_plot_comm_joy_ldist(
  pcon,
  length_min = 0,
  length_max = 1e+06,
  year_start = 1000,
  year_end = 9999,
  mfdb_gear_codes = c("BMT", "DSE", "LLN", "GIL"),
  sampling_types = c(1, 2, 4, 8),
  max_height = 50,
  split_by_sex = FALSE
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- length_min:

  Numeric. Minimum length to include (exclusive). Default is `0`.

- length_max:

  Numeric. Maximum length to include (exclusive). Default is `1e6`.

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`.

- mfdb_gear_codes:

  Character vector of MFDB gear codes to include. Default is
  `c("BMT", "DSE", "LLN", "GIL")`.

- sampling_types:

  Integer vector of sampling type codes. Default is `c(1, 2, 4, 8)`.

- max_height:

  Numeric. Scaling factor for ridge height. Default is `50`.

- split_by_sex:

  Logical. If `TRUE`, produces separate ridges for each sex. Default is
  `FALSE`.

## Value

A `ggplot2` plot object.
