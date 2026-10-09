# Extract length–weight data from a pax database

Queries the `station` and `aldist` tables in a pax database connection
to obtain individual length and weight measurements. When
`prediction_length_range` is supplied, a GAM is fitted to the observed
data and predicted weights are returned instead of raw observations.

## Usage

``` r
hr_input_data_lw(pcon, sampling_type = 30, prediction_length_range = NULL)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- sampling_type:

  Integer vector of sampling type codes to include. Default is `30`
  (spring groundfish survey).

- prediction_length_range:

  Numeric vector of lengths at which to predict weight using a GAM. If
  `NULL` (the default), the raw observed data are returned.

## Value

A tibble with columns `species`, `length`, and `weight`. If
`prediction_length_range` is provided, each row corresponds to a
predicted weight at the specified length.
