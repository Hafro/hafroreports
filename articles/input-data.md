# Input data for SAM and Muppet

The age-based assessments (SAM or Muppet) take one table, `input_data`,
with a row per year and age: catch in numbers and catch weights, the
survey indices, stock weights, maturity and natural mortality. In a
stock repository it is built by a chain of targets in
`script_assessment_model.R`:

| Target | Function | Gives |
|----|----|----|
| `pax_db` | [`pax::pax_from_mar()`](https://rdrr.io/pkg/pax/man/pax_from_mar.html) | the local DuckDB with stations, length and age samples, landings |
| `input_data_lw_pred` | [`hr_input_data_lw()`](https://hafro.github.io/hafroreports/reference/hr_input_data_lw.md) | weight at length (GAM fit to the survey) |
| `input_data_maturity_key` | [`hr_input_data_maturity_key()`](https://hafro.github.io/hafroreports/reference/hr_input_data_maturity_key.md) | maturity at length (and age) |
| `input_data_igfs_index`, `input_data_agfs_index` | [`hr_input_data_si_index()`](https://hafro.github.io/hafroreports/reference/hr_input_data_si_index.md) | survey abundance, weight and maturity at age |
| `input_data_comm_index` | [`hr_input_data_si_index()`](https://hafro.github.io/hafroreports/reference/hr_input_data_si_index.md) | catch at age (commercial samples raised to landings) |
| `input_data_landings` | [`hr_input_data_landings()`](https://hafro.github.io/hafroreports/reference/hr_input_data_landings.md) | landings by year |
| `input_data` | [`hr_input_data_combine()`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md) | the model input table |
| `sam_dat` | [`hr_sam_dat()`](https://hafro.github.io/hafroreports/reference/hr_sam_dat.md) | the SAM data object |

`02-had-targets` is the template; `03-sai`, `23-ple`, `08-usk` and
`01-cod` show how the arguments differ between stocks. This vignette
runs the chain on a small simulated database, so it builds without the
MFRI database.

``` r

library(hafroreports)
```

## The pax database

In a stock repository, `pax_db` is built from the MFRI database (Oracle,
through the `mar` package) once, or opened from a copy:

``` r

tar_target(
  pax_db,
  if (nzchar(Sys.getenv("PAX_SOURCE_DB"))) {
    pax_connect(Sys.getenv("PAX_SOURCE_DB"))
  } else {
    pax_from_mar(species, year_start, year_end)
  },
  format = pax_tar_format_duckdb()
)
```

Here we build a toy database with the same table names (`station`,
`ldist`, `aldist`, `measurement`, `landings`, `lw_coeffs`) from
simulated fish. The code is long only because it simulates the samples;
the point is that every `hr_input_data_*()` function takes a pax
connection and works on these tables.

``` r

# A toy pax database: a simulated stock ("species 99"), not real data.
# Ten years of a spring survey at 12 fixed stations in two strata, and 12
# commercial samples a year from two gears. pax_from_mar() builds the same
# tables (with many more columns) from the MFRI database.
set.seed(1)
years <- 2015:2024
ages <- 1:10
toy_species <- 99
# Numbers at age, von Bertalanffy growth and a maturity ogive at length
n_at_age <- function() 1000 * exp(-0.35 * (ages - 1) + rnorm(length(ages), 0, 0.3))
vb_length <- function(age) 90 * (1 - exp(-0.18 * (age + 0.3)))
p_mature <- function(length) stats::plogis((length - 45) / 4)
sim_fish <- function(n, l50 = 0) {
  age <- sample(ages, n, replace = TRUE, prob = n_at_age())
  length <- round(vb_length(age) * exp(rnorm(n, 0, 0.08)))
  keep <- runif(n) < stats::plogis((length - l50) / 3) # gear selection
  data.frame(age = age[keep], length = length[keep])
}

# Fixed survey stations: grid cells in divisions 101 and 107, with the
# stratum of each station and the stratum area (square nautical miles)
gridcells <- c(4211, 4213, 4221, 4222, 4141, 4142)
toy_strata_stations <- data.frame(
  station = 1:12,
  stratum = rep(1:2, each = 6),
  area = rep(c(3000, 5000), each = 6)
)

station <- ldist <- otoliths <- list()
sample_id <- 0
add_sample <- function(st, fish, n_aged, maturity) {
  sample_id <<- sample_id + 1
  id <- as.character(sample_id)
  station[[id]] <<- data.frame(sample_id = id, st)
  fish$sex <- sample(1:2, nrow(fish), replace = TRUE)
  ldist[[id]] <<- dplyr::count(
    data.frame(sample_id = id, species = toy_species, fish),
    sample_id, species, length, sex, name = "count"
  )
  aged <- fish[sample(nrow(fish), min(n_aged, nrow(fish))), ]
  otoliths[[id]] <<- data.frame(
    sample_id = id, species = toy_species, measurement_type = "OTOL",
    aged, count = 1,
    maturity_stage = if (maturity) ifelse(runif(nrow(aged)) < p_mature(aged$length), 2, 1) else NA
  )
}
for (y in years) {
  for (s in toy_strata_stations$station) {
    add_sample(
      data.frame(year = y, month = 3, station = s, sampling_type = 30,
        mfdb_gear_code = "BMT", gear_id = 73, tow_number = s,
        gridcell = gridcells[(s - 1) %% 6 + 1], tow_length = 4, tow_depth = 100),
      sim_fish(rpois(1, 60)), n_aged = 10, maturity = TRUE
    )
  }
  for (k in 1:12) {
    gear <- sample(c("BMT", "LLN"), 1)
    add_sample(
      # NB: for commercial samples tow_number is the haul number
      data.frame(year = y, month = sample(1:12, 1), station = NA, sampling_type = 1,
        mfdb_gear_code = gear, gear_id = if (gear == "BMT") 6 else 1,
        tow_number = sample(1:60, 1), gridcell = sample(gridcells, 1),
        tow_length = NA, tow_depth = 150),
      sim_fish(150, l50 = if (gear == "BMT") 40 else 50), n_aged = 25, maturity = FALSE
    )
  }
}
station <- dplyr::bind_rows(station)
ldist <- dplyr::bind_rows(ldist)
measurement <- dplyr::bind_rows(otoliths)
aldist <- measurement |>
  dplyr::mutate(weight = 0.01 * length^3) |>
  dplyr::count(sample_id, species, length, age, weight, name = "count")
landings <- tidyr::expand_grid(
  year = years, month = 1:12, mfdb_gear_code = c("BMT", "LLN")
) |>
  dplyr::mutate(
    species = toy_species, ices_area = "5a", country = "Iceland",
    boat_id = sample(1:30, dplyr::n(), replace = TRUE),
    catch = round(runif(dplyr::n(), 50e3, 150e3)) # kg
  )
lw_coeffs <- data.frame(species = toy_species, a = 0.01, b = 3)

pax_db <- pax::pax_connect(tempfile(fileext = ".duckdb"))
for (tbl_name in c("station", "ldist", "aldist", "measurement", "landings", "lw_coeffs")) {
  pax::pax_import(pax_db, get(tbl_name), name = tbl_name)
}
```

``` r

dplyr::tbl(pax_db, "station") |> dplyr::count(sampling_type) |> dplyr::collect()
#> # A tibble: 2 × 2
#>   sampling_type     n
#>           <dbl> <dbl>
#> 1             1   120
#> 2            30   120
```

## Weight and maturity keys

The weight-at-length key is a GAM fitted to the survey otoliths:

``` r

lw <- hr_input_data_lw(pax_db, sampling_type = 30, prediction_length_range = 1:120)
lw[lw$length %in% c(20, 40, 60), ]
#> # A tibble: 3 × 3
#>   species length weight
#>     <dbl>  <int>  <dbl>
#> 1      99     20   80.0
#> 2      99     40  640. 
#> 3      99     60 2160.
```

The maturity key fits a logistic model of maturity at length to the
survey otoliths. Measured maturity at age is kept, with the model
predictions marked by `age = NA`:

``` r

mat_key <- hr_input_data_maturity_key(pax_db, lgroups = seq(0, 120, 5))
mat_key |> dplyr::filter(is.na(age), lgroup %in% c(30, 45, 60))
#> # A tibble: 3 × 5
#>    year lgroup   age region  mat_p
#>   <dbl>  <dbl> <dbl> <chr>   <dbl>
#> 1    NA     30    NA all    0.0474
#> 2    NA     45    NA all    0.724 
#> 3    NA     60    NA all    0.978
```

The options the stocks use:

- `by_year = TRUE` fits a length + year model (`03-sai`, `23-ple`), with
  `copy_years` for years without data.
- `regions` with more than one region fits `log(lgroup) * region`; with
  one region the formula has no region term (earlier versions failed
  with “contrasts can be applied only to factors with 2 or more
  levels”).
- `sex = 2`, `mature_above`, `immature_below` reproduce the plaice key
  (`23-ple`).
- `aged_only = FALSE` also uses otoliths with a maturity stage but no
  age. pax relabels otoliths without an otolith number as `LEN`, so
  check where the maturity samples are in your stock’s data.

## Survey indices

[`hr_input_data_si_index()`](https://hafro.github.io/hafroreports/reference/hr_input_data_si_index.md)
raises the length distributions of each tow to the tow area, scales them
to the strata, splits them into ages with an age-length key and sums
abundance (`n`, thousands), mean weight (`mw`, g) and maturity (`mat`)
by year and age.

Strata can be assigned from the tow position (`strata_name` alone,
[`pax::pax_si_scale_by_strata()`](https://rdrr.io/pkg/pax/man/pax_si.html)),
or from a fixed station list (`strata_stations`). The old tidypax
indices used the station list (`biota.strata_stations`, in `pax_db` as
the `strata_stations` table), and assigning by position changed the
saithe spring survey index by -47% to +64% in single years. Use the
station list to reproduce a published index. Here the toy station list
carries the stratum areas itself:

``` r

smb <- hr_input_data_si_index(
  pax_db,
  sampling_type = 30,
  lw_key = lw,
  maturity_key = mat_key,
  regions = list(all = 101:115),
  strata_stations = toy_strata_stations
)
smb |> dplyr::filter(year == 2024) |> dplyr::collect() |> head(4)
#> # A tibble: 4 × 5
#> # Groups:   year [1]
#>    year   age     n     mw   mat
#>   <int> <int> <dbl>  <dbl> <dbl>
#> 1  2024     1  3.43   70.0 0    
#> 2  2024     2  2.39  286.  0    
#> 3  2024     3  2.15  744.  0.329
#> 4  2024     4  1.39 1299.  0.533
```

In a stock repository the station list comes from `pax_db` and the areas
from the pax strata table:

``` r

hr_input_data_si_index(
  pax_db,
  sampling_type = 30,
  tow_number = 0:35,
  lw_key = input_data_lw_pred,
  maturity_key = input_data_maturity_key,
  strata_name = "old_strata",
  strata_stations = strata_stations |>
    dplyr::filter(sampling_type == 30, stratification == "old_strata"),
  regions = list(all = 101:115)
)
```

Other survey options: `gear_id_filter` and `tow_number` select the index
tows; `haul_scalar` down-weights named very large hauls (`03-sai`);
`ygroup` pools years with few otoliths in the age-length key (`23-ple`),
and `alk` or `key_year` give a key the function can’t build. Use
[`hr_pool_years()`](https://hafro.github.io/hafroreports/reference/hr_pool_years.md)
rather than pax’s own `ygroup`, which fails in DuckDB.

## Catch at age

The same function gives the catch at age from the commercial samples: an
age-length key by gear group and half year, raised to the landings with
`scale_by_landings = TRUE`.

Don’t give a `tow_number` filter for commercial samples. For them it is
the haul number, which can exceed 35; the template’s `tow_number = 0:35`
dropped 3–14% of the saithe and haddock samples in recent years and
shifted the catch at age. In the toy data:

``` r

dplyr::tbl(pax_db, "station") |>
  dplyr::filter(sampling_type == 1) |>
  dplyr::summarise(samples = dplyr::n(), haul_above_35 = sum(ifelse(tow_number > 35, 1, 0))) |>
  dplyr::collect()
#> # A tibble: 1 × 2
#>   samples haul_above_35
#>     <dbl>         <dbl>
#> 1     120            43
```

``` r

comm <- hr_input_data_si_index(
  pax_db,
  sampling_type = 1,
  lw_key = lw,
  tgroup = list(t1 = 1:6, t2 = 7:12),
  gear_group = list(BMT = "BMT", LLN = "LLN"),
  scale_by_landings = TRUE
)
comm |> dplyr::filter(year == 2024) |> dplyr::collect() |> head(4)
#> # A tibble: 4 × 5
#> # Groups:   year [1]
#>    year   age      n    mw   mat
#>   <int> <int>  <dbl> <dbl> <int>
#> 1  2024     2   1.37  328.    NA
#> 2  2024     3 222.    756.    NA
#> 3  2024     4 207.   1311.    NA
#> 4  2024     5 213.   1859.    NA
```

Missing values in the samples or the landings are dropped silently
unless you say what they are. The function reports how many samples it
leaves out:

- `gridcell_na`: samples without a position match no region (plaice lost
  the whole of 1990 without `gridcell_na = 2741`).
- `sample_gear_na`, `landings_gear_na`, `landings_month_na`: what
  unknown gears and months were in the old scripts (saithe: DSE for
  samples, BMT and month 6 for landings).
- [`pax::pax_add_other()`](https://rdrr.io/pkg/pax/man/pax_add_groupings.html)
  in `gear_group` gives a default group, for gears outside the listed
  groups.
- `sam_use_10_11_first_2_years = TRUE` adds survey otoliths to the
  commercial key in the first two years. It is a haddock choice; saithe
  and plaice don’t use it.

## Combine

[`hr_input_data_combine()`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md)
joins the indices and landings, fills gaps and scales the catch so that
catch × catch weight equals the landings each year. With the defaults it
only fills weights with the mean for the age; the stock’s own rules are
arguments (maturity cut-offs, values before the survey started, running
means, M), as in `03-sai`:

``` r

hr_input_data_combine(
  year_start, year_end,
  age_start = 0,
  input_data_comm_index = input_data_comm_index,
  input_data_igfs_index = input_data_igfs_index,
  input_data_agfs_index = input_data_agfs_index,
  input_data_landings = input_data_landings,
  M = 0.2,
  immature_below_age = 4,
  mature_above_age = 9,
  maturity_fixed = data.frame(age = 4:9, maturity = c(0.08, 0.185, 0.371, 0.61, 0.79, 0.912), year_before = 1985),
  maturity_smooth_years = 5,
  weight_fill_year = 1985,
  stock_weight_smooth_above = 8,
  stock_weight_smooth_years = 4
)
```

The toy stock has no autumn survey, so we pass an empty table:

``` r

input_data <- hr_input_data_combine(
  year_start = 2015,
  year_end = 2024,
  age_start = 1,
  input_data_comm_index = comm,
  input_data_igfs_index = smb,
  input_data_agfs_index = data.frame(year = numeric(), age = numeric(), n = numeric()),
  input_data_landings = hr_input_data_landings(pax_db),
  immature_below_age = 2,
  mature_above_age = 7
)
input_data |> dplyr::filter(year == 2024) |> head(5)
#> # A tibble: 5 × 9
#>    year   age  catch catch_weight   smb stock_weight maturity   smh     M
#>   <dbl> <dbl>  <dbl>        <dbl> <dbl>        <dbl>    <dbl> <dbl> <dbl>
#> 1  2024     1   0            106. 3.43          70.0    0        NA   0.2
#> 2  2024     2   1.37         328. 2.39         286.     0        NA   0.2
#> 3  2024     3 222.           756. 2.15         744.     0.329    NA   0.2
#> 4  2024     4 207.          1311. 1.39        1299.     0.533    NA   0.2
#> 5  2024     5 213.          1859. 0.989       1876.     0.930    NA   0.2
```

Check that catch × catch weight gives the landings (kg):

``` r

input_data |>
  dplyr::group_by(year) |>
  dplyr::summarise(catch_kg = sum(catch * catch_weight, na.rm = TRUE)) |>
  dplyr::left_join(dplyr::collect(hr_input_data_landings(pax_db)), by = "year") |>
  head(3)
#> # A tibble: 3 × 3
#>    year catch_kg   catch
#>   <dbl>    <dbl>   <dbl>
#> 1  2015  2576596 2576596
#> 2  2016  2313963 2313963
#> 3  2017  2293057 2293057
```

Leave `age_end = NULL` (all ages in the data) and let the model form the
plus group. Cutting ages here drops the old fish, and scaling to
landings then moves their weight onto the younger ages: saithe SSB was
about 30% too low.

## SAM

[`hr_sam_dat()`](https://hafro.github.io/hafroreports/reference/hr_sam_dat.md)
turns `input_data` into a SAM data object, one `hr_sam_*()` helper per
matrix. The defaults are the spring survey only and no F or M before
spawning; give the stock’s values explicitly (haddock: `pf = 0.4`,
`pm = 0.3`). The wrong proportions put saithe SSB 14% low while every
input matched.

``` r

model_dat <- input_data |> dplyr::filter(year <= 2023)
sam_dat <- hr_sam_dat(model_dat = model_dat, minage = 1, maxage = 10)
names(sam_dat)[1:5]
```

The configuration
([`hr_sam_conf()`](https://hafro.github.io/hafroreports/reference/hr_sam_conf.md))
is the haddock one; each stock keeps its own `*_sam_conf()` in
`R/sam.R`, written from the old `model_setup.R`. The fit itself is
`SAMutils::full_sam_fit(sam_dat, sam_conf)`. The SAM chunks above run
only when SAMutils and stockassessment are installed.

## Muppet

[`hr_muppet_input_datafiles()`](https://hafro.github.io/hafroreports/reference/hr_muppet_input_datafiles.md)
writes the same table as the Muppet data files (`01-cod`).
`haddock_fills = FALSE` writes missing weights and maturity as missing
(-1) instead of haddock values:

``` r

files <- hr_muppet_input_datafiles(
  input_data,
  year_start = 2015,
  year_end = 2024,
  age_end = 10,
  haddock_fills = FALSE,
  smb_year_start = 2015,
  smh_year_start = 2015,
  survey_ages = 1:10
)
names(files)
#> [1] "Files/catchandstockdata.dat" "Files/totcatch.dat"         
#> [3] "Files/marsurveydata.dat"     "Files/autsurveydata.dat"
cat(head(strsplit(files[["Files/catchandstockdata.dat"]], "\n")[[1]], 4), sep = "\n")
#> # year age cno cwt swt mat ssbwt
#> 2015 1   -1  -1  70.24412017181616   0   70.24412017181616
#> 2015 2   12.52220249537648   352.36402000729663  307.8869609180586   0.11325678496868477 307.8869609180586
#> 2015 3   126.01052474897135  831.4700686632701   670.3893043724285   0.48659198913781376 670.3893043724285
```

## AI use

This vignette was drafted with Claude (Anthropic) in October 2026 and
has not yet been checked by a person (MFRI policy on AI use).
