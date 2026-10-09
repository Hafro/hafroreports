# Aggregate total landings by year from a pax database

Queries the `landings` table and returns the sum of `catch` for each
year.

## Usage

``` r
hr_input_data_landings(pcon)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

## Value

A lazy tibble (or tibble after collection) with columns `year` and
`catch` (total catch in the units stored in the database).
