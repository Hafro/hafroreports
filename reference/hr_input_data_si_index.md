# Compute survey index data (abundance and biomass at age)

Derives age-structured survey abundance (thousands) and mean weight (g)
from a pax database by applying a length distribution, an age–length
key, optional strata scaling, and optional maturity weighting. The
result is the primary model input table used by the SAM and MUPPET
assessment workflows. Used for both surveys and commercial samples.

## Usage

``` r
hr_input_data_si_index(
  pcon,
  lw_key = NULL,
  maturity_key = NULL,
  strata_name = NULL,
  sampling_type = 30,
  sam_use_10_11_first_2_years = FALSE,
  tow_number = NULL,
  tgroup = NULL,
  regions = list(all = 101:115),
  lgroups = seq(0, 200, 5),
  gear_group = NULL,
  gear_id_filter = NULL,
  scale_by_landings = FALSE,
  haul_scalar = NULL,
  strata_stations = NULL,
  maturity_measured = TRUE,
  maturity_na = NULL,
  ygroup = NULL,
  gridcell_na = NULL,
  sample_gear_na = NULL,
  landings_gear_na = NULL,
  landings_month_na = NULL,
  ygroup_alk = ygroup,
  alk_age_max = NULL,
  alk = NULL,
  key_year = NULL,
  landings_area_like = NULL,
  plus_group = NULL,
  mean_length = FALSE,
  gear_group_alk = gear_group,
  sample_gear_na_alk = NULL
)
```

## Arguments

- pcon:

  A database connection object compatible with
  [`dplyr::tbl`](https://dplyr.tidyverse.org/reference/tbl.html).

- lw_key:

  Data frame with columns `species`, `length`, and `weight` for joining
  weight-at-length, and optionally `region` for weights by region (of
  `regions`). If `NULL`, weights are derived directly from the `ldist`
  table.

- maturity_key:

  Output of
  [`hr_input_data_maturity_key`](https://hafro.github.io/hafroreports/reference/hr_input_data_maturity_key.md),
  used to compute maturity-weighted biomass. If `NULL`, no maturity
  column is produced.

- strata_name:

  Character. Name of the stratification scheme to use for survey scaling
  (passed to
  [`pax::pax_si_scale_by_strata`](https://rdrr.io/pkg/pax/man/pax_si.html)).
  If `NULL`, no strata scaling is applied.

- sampling_type:

  Integer vector of sampling type codes for station filtering. Default
  is `30`.

- sam_use_10_11_first_2_years:

  Logical. If `TRUE`, sampling types 10 and 11 are additionally included
  in the age-length key for the first two years of data to improve age-1
  estimates. Default is `FALSE`.

- tow_number:

  Integer vector of valid tow numbers (NA coerced to 0), or `NULL` for
  no filtering. Use for surveys (e.g. `0:35` for the spring survey); for
  commercial samples `tow_number` is the haul number, which can
  exceed 35. Default is `NULL`.

- tgroup:

  Named list of months, e.g. `list(t1 = 1:6, t2 = 7:12)`, or `NULL`.
  Default is `NULL`.

- regions:

  Named list mapping region labels to integer MFDB area codes. Default
  is `list(all = 101:115)`.

- lgroups:

  Numeric vector of length group break points. Default is
  `seq(0, 200, 5)`.

- gear_group:

  Named list mapping gear group labels to MFDB gear codes, or `NULL` for
  no gear grouping. Samples with other gears are left out (with a
  message) unless a group is
  [`pax::pax_add_other()`](https://rdrr.io/pkg/pax/man/pax_add_groupings.html).
  Default is `NULL`.

- gear_id_filter:

  Integer vector of gear IDs to include, or `NULL` for no filtering.
  Default is `NULL`.

- scale_by_landings:

  Logical. If `TRUE`, indices are additionally scaled to match landings
  by gear group and time group. Default is `FALSE`.

- haul_scalar:

  Data frame with columns `sample_id` and `scalar`, to down-weight
  individual (e.g. very large) hauls. Default `NULL`.

- strata_stations:

  Data frame with columns `station` and `stratum`. If given, strata are
  assigned from it rather than from tow positions, see
  [`hr_si_scale_by_strata_stations`](https://hafro.github.io/hafroreports/reference/hr_si_scale_by_strata_stations.md).
  Default `NULL`.

- maturity_measured:

  Logical. If `FALSE`, only the modelled maturity at length from
  `maturity_key` is used, not the measured maturity at age. Default
  `TRUE`.

- maturity_na:

  Proportion mature for lengths with neither a measurement nor an
  estimate, or `NULL` to leave them out. Default `NULL`.

- ygroup:

  Named list of years to pool in the age-length key, e.g.
  `list(past = 1980:1994)`, see
  [`hr_pool_years`](https://hafro.github.io/hafroreports/reference/hr_pool_years.md).
  Default `NULL`.

- gridcell_na:

  Grid cell to give samples without a position. Without a position a
  sample gets no region, matches no age-length key cell and is dropped
  (with a message giving the number of samples). Default `NULL`.

- sample_gear_na:

  Gear code to give samples with unknown gear, when raising them (not in
  the age-length key). Default `NULL`.

- landings_gear_na, landings_month_na:

  Gear code and month to give landings with unknown gear or month when
  `scale_by_landings = TRUE`. Landings without a month are otherwise
  left out of the scaling. Default `NULL`.

- ygroup_alk:

  Named list of years to pool when building the age-length key, when it
  differs from `ygroup` (the pools the index uses). Default `ygroup`.

- alk_age_max:

  Otoliths older than this are left out of the age-length key. Default
  `NULL`, all.

- alk:

  A precomputed age-length key (as
  [`pax::pax_ldist_alk()`](https://rdrr.io/pkg/pax/man/pax_ldist.html):
  columns `ygroup` for the key year, `lgroup`, `age`, `agep` and the
  grouping columns `region`, `gear_name`, `tgroup`, `species`), for keys
  this function can't build (blended, borrowed from other years, shifted
  ages). Default `NULL`, the key is built from the samples' otoliths.

- key_year:

  Named vector mapping each year (names) to the year of the key it uses
  (values), e.g. a pooled or another survey's key. Years not in it are
  dropped. The key years can be numbers or labels (e.g. `"past"`), as in
  the key's `ygroup`. Not with `ygroup`. Default `NULL`.

- landings_area_like:

  SQL LIKE pattern of the ICES areas of the landings to scale to
  (`scale_by_landings = TRUE`), when the pax database holds landings
  from more areas. Default `NULL`, all.

- plus_group:

  Ages above this are summed into it. Default `NULL`.

- mean_length:

  If `TRUE`, also the mean length at age (`ml`). Default `FALSE`.

- gear_group_alk:

  Gear groups of the age-length key, when they differ from `gear_group`
  (which then only groups the raising to landings). Default
  `gear_group`.

- sample_gear_na_alk:

  Gear code to give samples with unknown gear in the age-length key.
  Default `NULL`.

## Value

A grouped tibble with columns `year`, `age`, `n` (abundance in
thousands), `mw` (mean weight in grams), and optionally `mat`
(proportion mature).
