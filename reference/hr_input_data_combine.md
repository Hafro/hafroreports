# Combine survey indices into single input_data

Combines commercial catch-at-age, spring (IGFS/SMB) and autumn
(AGFS/SMH) survey indices, and total landings into a single data frame
suitable for passing to
[`hr_sam_dat`](https://hafro.github.io/hafroreports/reference/hr_sam_dat.md)
or
[`hr_muppet_input_datafiles`](https://hafro.github.io/hafroreports/reference/hr_muppet_input_datafiles.md).
Stock-specific rules for filling gaps and smoothing are set by the
arguments; with the defaults only the joins, catch scaled to landings
and natural mortality are applied.

## Usage

``` r
hr_input_data_combine(
  year_start,
  year_end,
  age_start = 0,
  age_end = NULL,
  input_data_comm_index,
  input_data_igfs_index,
  input_data_agfs_index,
  input_data_landings,
  M = 0.2,
  immature_below_age = NULL,
  mature_above_age = NULL,
  maturity_fixed = NULL,
  maturity_fill_age_mean = TRUE,
  maturity_mean_mature_above = NULL,
  weight_fill_year = NULL,
  weights_fixed = NULL,
  stock_weight_smooth_above = NULL,
  stock_weight_smooth_years = NULL,
  maturity_smooth_years = NULL,
  catch_weight_fill_year = weight_fill_year,
  stock_weight_fill_year = weight_fill_year,
  catch_weight_fill_mean = TRUE,
  stock_weight_fill_mean = TRUE,
  stock_survey = c("igfs", "agfs")
)
```

## Arguments

- year_start:

  Integer. First year to include.

- year_end:

  Integer. Last year to include.

- age_start:

  Integer. Minimum age. Default is `0`.

- age_end:

  Integer. Maximum age. Default `NULL` is the oldest age in the
  commercial and spring survey indices; the plus group is formed later
  (e.g. by `SAMutils::sam.input()`). Cutting ages here loses the older
  fish and inflates younger ages when scaling to landings.

- input_data_comm_index:

  Tibble. Commercial index with columns `year`, `age`, `n` (catch
  numbers), and `mw` (mean weight).

- input_data_igfs_index:

  Tibble. Spring groundfish survey index with columns `year`, `age`,
  `n`, `mw`, and `mat`.

- input_data_agfs_index:

  Tibble. Autumn groundfish survey index with columns `year`, `age`, and
  `n`.

- input_data_landings:

  Tibble. Annual total landings with columns `year` and `catch`.

- M:

  Natural mortality, all ages. Default `0.2`.

- immature_below_age, mature_above_age:

  Ages below / above which maturity is set to 0 / 1. Default `NULL`.

- maturity_fixed:

  Data frame with columns `age`, `maturity` and `year_before`: maturity
  for years before `year_before` (e.g. before the spring survey
  started). Default `NULL`.

- maturity_fill_age_mean:

  Logical. Fill missing maturity with the mean for the age over years.
  Default `TRUE`.

- maturity_mean_mature_above:

  Ages above which fish count as mature when computing that mean.
  Default `NULL`.

- weight_fill_year:

  Year whose weights fill missing catch and stock weights for the age
  (before the mean over years). Default `NULL`.

- weights_fixed:

  Data frame with columns `age`, `catch_weight` and `stock_weight` (g),
  applied after scaling to landings. Default `NULL`.

- stock_weight_smooth_above, stock_weight_smooth_years:

  Running mean of stock weights for ages above
  `stock_weight_smooth_above`. Default `NULL`.

- maturity_smooth_years:

  Running mean of maturity. Default `NULL`.

- catch_weight_fill_year, stock_weight_fill_year:

  The same for catch and stock weights separately. Default
  `weight_fill_year`.

- catch_weight_fill_mean, stock_weight_fill_mean:

  Fill (remaining) missing catch / stock weights with the mean over
  years for the age. `FALSE` leaves them missing (e.g. for SAM to fill).
  Default `TRUE`.

- stock_survey:

  Survey index giving the stock weights and maturity: `"igfs"` (spring,
  default) or `"agfs"` (autumn; then `input_data_agfs_index` needs `mw`
  and `mat`). The other index gives only its numbers.

## Value

A tibble with columns `year`, `age`, `catch`, `catch_weight`, `smb`,
`stock_weight`, `maturity`, `smh`, and `M`, covering the requested year
and age range.

## Details

The steps, in order:

1.  maturity: `immature_below_age` / `mature_above_age`, then
    `maturity_fixed`, then missing values from the mean for the age
    (`maturity_fill_age_mean`)

2.  catch and stock weights: missing values from `weight_fill_year`,
    then the mean over years for the age

3.  catch in numbers scaled so that catch x catch weight equals the
    landings each year

4.  `weights_fixed` (after scaling)

5.  running means: stock weights of ages above
    `stock_weight_smooth_above` over `stock_weight_smooth_years` years,
    maturity over `maturity_smooth_years` years (current and previous
    years, partial windows at the start)

6.  natural mortality `M`
