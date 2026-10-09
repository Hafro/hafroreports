# Plot CPUE time series by gear group

Queries the `logbook` table, groups gears according to `gear_group`,
calculates catch per unit effort for each gear, and produces a faceted
CPUE time series plot.

## Usage

``` r
hr_techreport_plot_cpue(
  pcon,
  gear_group = list(GIL = "GIL", BMT = c("BMT", "NPT", "SHT", "PGT", "DRD"), LLN =
    c("HLN", "LLN"), DSE = c("PSE", "DSE")),
  year_start = 1000,
  year_end = 9999,
  limit = 0.5,
  effort_na = 1,
  drop_no_effort = TRUE,
  hooks_na = 10000
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- gear_group:

  Named list mapping gear group labels to vectors of MFDB gear codes.
  Default groups are `GIL`, `BMT`, `LLN`, and `DSE`.

- year_start:

  Integer. First year to include. Default is `1000`.

- year_end:

  Integer. Last year to include. Default is `9999`.

- limit:

  Share of the species in the catch above which records count as
  directed (the dashed lines), passed to
  [`pax::pax_logbook_cpue_plot()`](https://rdrr.io/pkg/pax/man/pax_logbook.html).
  Default `0.5`.

- effort_na:

  Effort of records with no tow time, hooks or nets (a bottom trawl haul
  without tow time), passed to
  [`pax::pax_add_cpue()`](https://rdrr.io/pkg/pax/man/pax_logbook.html).
  Default `1` (one hour); `NULL` leaves them out.

- drop_no_effort:

  If `TRUE` (default), records with none of hooks, nets or tow time are
  left out before `effort_na` applies (Danish seine records count as one
  set).

- hooks_na:

  Number of hooks given to records without, of any gear. Default
  `10000`, as before: a bottom trawl haul without tow time then has an
  effort of 10. `NULL` leaves them without hooks, so `effort_na`
  applies.

## Value

A `ggplot2` plot object faceted by gear group.
