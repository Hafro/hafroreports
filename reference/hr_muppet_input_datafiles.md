# Prepare MUPPET input data files

Converts a model input data frame into the tab-delimited text files
expected by MUPPET: `Files/catchandstockdata.dat`, `Files/totcatch.dat`,
`Files/marsurveydata.dat` (spring SMB), and `Files/autsurveydata.dat`
(autumn SMH). Hard-coded fallback weight and maturity vectors are
applied when data are missing.

## Usage

``` r
hr_muppet_input_datafiles(
  assessment_input_data,
  year_start,
  year_end,
  age_end,
  age_start = 1,
  haddock_fills = TRUE,
  smb_year_start = 1985,
  smh_year_start = 1996,
  survey_ages = 1:13,
  smh_skip_years = 2011
)
```

## Arguments

- assessment_input_data:

  A data frame with columns `year`, `age`, `catch`, `catch_weight`,
  `stock_weight`, `maturity`, `smb`, and `smh`, as produced by
  [`hr_input_data_combine`](https://hafro.github.io/hafroreports/reference/hr_input_data_combine.md).

- year_start:

  Integer. First year to include in the output files.

- year_end:

  Integer. Last year (assessment year). Catch and catch weight are set
  to `-1` for this year as the data are incomplete.

- age_end:

  Integer. Maximum age to include.

- age_start:

  Integer. Minimum age to include. Default is `1`.

- haddock_fills:

  If `TRUE` (default), missing stock weights and maturity are filled
  with fixed haddock values by age, and missing catch weights with 4000
  g (age 2: 600 g). `FALSE` writes them as missing (-1), e.g. for
  another stock whose input data are complete.

- smb_year_start, smh_year_start:

  First years of the spring and autumn survey files. Default `1985` and
  `1996`.

- survey_ages:

  Ages of the survey files. Default `1:13`.

- smh_skip_years:

  Years of the autumn survey written as missing. Default `2011` (no
  survey).

## Value

A named list of character strings, each being the formatted content of a
MUPPET input file. Names are the file paths relative to the MUPPET run
directory.
