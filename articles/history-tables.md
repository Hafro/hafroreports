# History tables: advice, TAC and assessment summaries

The advice sheets and tech reports show history: the advice and TAC of
each year (TAC table, management figure) and the assessment summaries of
the last few years (retrospective figure). Each stock repository keeps
them as CSV files in `data/` and adds this year’s rows in the pipeline:

| File | Target | Rows |
|----|----|----|
| `data/advice_hist.csv` | `advice_hist` | one per advice year: `assessment_year`, `advice_period`, `advice`, `advice_basis.en/is` |
| `data/tac_hist.csv` | `tac_hist` | one per year and area: `assessment_year`, `ices_area`, `tac` |
| `data/assessment.csv` | `assessment_hist` | one per assessment year and year: the columns of [`hr_assessment_template()`](https://hafro.github.io/hafroreports/reference/hr_assessment_template.md) |
| `data/projections_at_age.csv` | `projections_at_age_hist` | (not filled yet) |

``` r

library(hafroreports)
```

## hr_update_hist()

[`hr_update_hist()`](https://hafro.github.io/hafroreports/reference/hr_update_hist.md)
reads CSV files or takes data frames and merges them by
`assessment_year`: rows of a later argument replace rows of an earlier
one with the same `assessment_year`, and rows with
`assessment_year = NA` (templates) are dropped at the end.

``` r

dir <- tempfile()
dir.create(dir)
advice_file <- file.path(dir, "advice_hist.csv")
writeLines(c(
  "assessment_year,advice_period,advice,advice_basis.en,advice_basis.is",
  "2023,2023/2024,1200,Management plan,Aflaregla",
  "2024,2024/2025,1350,Management plan,Aflaregla",
  "2025,2025/2026,1410,Management plan,Aflaregla"
), advice_file)

this_year <- data.frame(
  assessment_year = 2026,
  advice_period = "2026/2027",
  advice = 1520,
  advice_basis.en = "Management plan",
  advice_basis.is = "Aflaregla"
)
hr_update_hist(advice_file, this_year)
#> # A tibble: 4 × 5
#>   assessment_year advice_period advice advice_basis.en advice_basis.is
#>             <dbl> <chr>          <dbl> <chr>           <chr>          
#> 1            2023 2023/2024       1200 Management plan Aflaregla      
#> 2            2024 2024/2025       1350 Management plan Aflaregla      
#> 3            2025 2025/2026       1410 Management plan Aflaregla      
#> 4            2026 2026/2027       1520 Management plan Aflaregla
```

That is what the template `02-had-targets` does. It has one problem: the
pipeline is rerun after the advice is published, e.g. on next year’s
data for a benchmark or a check. Then the run’s `assessment_year` is
still the year of the last published advice, and
[`hr_update_hist()`](https://hafro.github.io/hafroreports/reference/hr_update_hist.md)
replaces the published row with the rerun’s value:

``` r

rerun <- transform(this_year, assessment_year = 2025, advice_period = "2025/2026", advice = 1433)
hr_update_hist(advice_file, rerun) |> dplyr::filter(assessment_year == 2025)
#> # A tibble: 1 × 5
#>   assessment_year advice_period advice advice_basis.en advice_basis.is
#>             <dbl> <chr>          <dbl> <chr>           <chr>          
#> 1            2025 2025/2026       1433 Management plan Aflaregla
```

The advice sheet then shows a number that was never advised (cod:
201,355 t instead of the published 201,674 t).

## update_hist(): published rows never change

Every stock repository except the template therefore has `R/hist.R`,
with the same `update_hist()` (`01-cod`, `03-sai`, `04-whg`, `23-ple`,
`25-wit`, and the others):

``` r

update_hist <- function(hist, new, template = NULL) {
  # NSE variables
  assessment_year <- NULL

  hist <- hr_update_hist(template, hist)
  values <- setdiff(names(hist)[vapply(hist, is.numeric, logical(1))], "assessment_year")
  placeholder <- if (length(values)) {
    rowSums(!is.na(as.data.frame(hist)[values])) == 0
  } else {
    rep(FALSE, nrow(hist))
  }
  hist <- hist[!(placeholder & hist$assessment_year %in% new$assessment_year), ]
  new <- dplyr::filter(new, !(assessment_year %in% hist$assessment_year))
  dplyr::bind_rows(hist, new) |>
    dplyr::arrange(assessment_year)
}
```

The run’s rows are added only for assessment years the file doesn’t
have:

``` r

update_hist(advice_file, rerun) |> dplyr::filter(assessment_year == 2025)
#> # A tibble: 1 × 5
#>   assessment_year advice_period advice advice_basis.en advice_basis.is
#>             <dbl> <chr>          <dbl> <chr>           <chr>          
#> 1            2025 2025/2026       1410 Management plan Aflaregla
update_hist(advice_file, this_year) |> dplyr::filter(assessment_year >= 2025)
#> # A tibble: 2 × 5
#>   assessment_year advice_period advice advice_basis.en advice_basis.is
#>             <dbl> <chr>          <dbl> <chr>           <chr>          
#> 1            2025 2025/2026       1410 Management plan Aflaregla      
#> 2            2026 2026/2027       1520 Management plan Aflaregla
```

Rows whose values are all missing are placeholders, not published
values, and are replaced. A TAC file often has the row of the coming
year before the TAC is set:

``` r

tac_file <- file.path(dir, "tac_hist.csv")
writeLines(c(
  "assessment_year,ices_area,tac",
  "2024,5a,1350",
  "2025,5a,1410",
  "2026,5a,NA"
), tac_file)
update_hist(
  tac_file,
  data.frame(assessment_year = 2026, ices_area = "5a", tac = 1520)
)
#> # A tibble: 3 × 3
#>   assessment_year ices_area   tac
#>             <dbl> <chr>     <dbl>
#> 1            2024 5a         1350
#> 2            2025 5a         1410
#> 3            2026 5a         1520
```

Without that, `05-reg`’s advice sheets said “NA tonnes”.

In the stock repository the targets look like this (`23-ple`):

``` r
tar_target(historical_advice_file, "data/advice_hist.csv", format = "file"),
tar_target(
  advice_hist,
  update_hist(
    historical_advice_file,
    data.frame(
      assessment_year = assessment_year,
      advice_period = paste(assessment_year, assessment_year + 1, sep = "/"),
      advice = attr(prognosis, "tac"),
      advice_basis.en = "Management plan",
      advice_basis.is = "Aflaregla"
    )
  )
)
```

## The assessment summary

`assessment_hist` holds the summary (recruitment, SSB, F or harvest
rate, reference biomass, landings, with intervals) of each assessment
year. Give
[`hr_assessment_template()`](https://hafro.github.io/hafroreports/reference/hr_assessment_template.md)
as `template`, by name, so the columns and their types are fixed even
when the file lacks some of them:

``` r

assessment_file <- file.path(dir, "assessment.csv")
writeLines(c(
  "year,species,median_SSB,assessment_year",
  "2023,99,52000,2025",
  "2024,99,55000,2025"
), assessment_file)
summary_2026 <- hr_assessment_template()[rep(1, 3), ]
summary_2026$year <- 2024:2026
summary_2026$species <- 99L
summary_2026$median_SSB <- c(54000, 57000, 60000)
summary_2026$assessment_year <- 2026L

assessment_hist <- update_hist(assessment_file, summary_2026, template = hr_assessment_template())
dim(assessment_hist)
#> [1]  5 19
table(assessment_hist$assessment_year)
#> 
#> 2025 2026 
#>    2    3
```

This year’s rows come from the fit, not from ICES SAG:
`hr_assessment_from_fit(sam_fit$fit, input_data_landings, species, assessment_year, ref_bio_type = "age")`
(`03-sai`, `23-ple`). SAG custom columns vary by stock and year (a
column named `F` once wiped the reference biomass), so
[`hr_assessment_from_sag()`](https://hafro.github.io/hafroreports/reference/hr_assessment_from_sag.md)
is only the fallback. Category 3 stocks build the rows from their
indices instead (`25-wit`: `wit_assessment_summary()` puts the biomass
index in `SSB`, the juvenile index in `recruitment` and the fishing
pressure proxy in `F`).

## Things to know

- **The files are read, not written.** The targets combine the files
  with the current run in memory. After the advice is published, add the
  published rows to `data/*.csv` by hand, and check that the latest
  assessment year is there (`19-aru` was missing 2026).
- **`assessment_year` is the year of the advice**, not the last catch
  year (`data_year`). Mixing them makes the advice recompute last year,
  or the retro figure fail (“Insufficient values in manual scale”).
- **Biennial advice** (rfb rule, `25-wit`) adds two rows,
  `assessment_year + 0:1`, with the same advice.
- **Header-only CSVs** read as character columns: pass `col_types`, e.g.
  `readr::read_csv(file, col_types = readr::cols(.default = "d"))` for
  `projections_at_age.csv` (`23-ple`).
- **Old files** may hold landings in kg for some years, and empty
  `refbio` rows for F-based stocks; drop the latter before
  [`hr_advice_plot_ssb()`](https://hafro.github.io/hafroreports/reference/hr_advice_plot_ssb.md).

## AI use

This vignette was drafted with Claude (Anthropic) in October 2026 and
has not yet been checked by a person (MFRI policy on AI use).
