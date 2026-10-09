# Scale survey abundance to strata using a station list

As
[`pax::pax_si_scale_by_strata()`](https://rdrr.io/pkg/pax/man/pax_si.html),
but assigns stations to strata with a fixed station list (e.g.
`biota.strata_stations`, as the tidypax-based assessments did) instead
of the h3 cell of the tow position. Tows near a stratum boundary
otherwise move between strata, which can change a survey index by tens
of percent in single years. Stratum areas come from the pax strata table
`strata_name`.

## Usage

``` r
hr_si_scale_by_strata_stations(tbl, strata_stations, strata_name = NULL)
```

## Arguments

- tbl:

  Output of
  [`pax::pax_si_by_length()`](https://rdrr.io/pkg/pax/man/pax_si.html).

- strata_stations:

  Data frame with columns `station` and `stratum`.

- strata_name:

  Name of the pax strata table holding stratum areas (`rall_area`,
  km^2).

## Value

`tbl` with `si_abund` and `si_biomass` scaled to the stratum area.
