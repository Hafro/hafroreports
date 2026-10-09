# Biomass reference points in tonnes

Reference points are usually kept with biomass in thousand tonnes, as
the advice figures plot biomass in thousand tonnes. This converts the
biomass reference points (names starting `B_` or ending `btrigger`) for
tables or plots in tonnes, leaving fishing mortality and harvest rates
as they are.

## Usage

``` r
hr_ref_points_tonnes(ref_points, multiplier = 1000)
```

## Arguments

- ref_points:

  Named list of reference points.

- multiplier:

  Multiplier for the biomass reference points. Default `1000`.

## Value

`ref_points` with the biomass reference points multiplied.
