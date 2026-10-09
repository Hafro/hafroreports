# Pool years for an age-length key

Relabels every year in each group of `ygroup` as the group's first year,
so the years share one age-length key. Used instead of the `ygroup`
argument of pax, which coalesces the (text) group name with the
(numeric) year and fails in DuckDB.

## Usage

``` r
hr_pool_years(tbl, ygroup)
```

## Arguments

- tbl:

  A (lazy) table with a `year` column.

- ygroup:

  Named list of year vectors, e.g. `list(past = 1980:1994)`. `NULL` does
  nothing.

## Value

`tbl` with `year` relabelled.
