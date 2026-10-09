# Assemble prognosis summary data for advice sheet

Collects the key prognosis metrics for a given assessment year into a
single-row tibble: basis text, management harvest rate, catch, SSB at
`assessment_year + 2`, SSB percentage change, and current and previous
TAC values.

## Usage

``` r
hr_advice_data_prognosis(
  basis_table,
  tac_hist,
  ref_points,
  stock_dev,
  assessment_year,
  fishing_pressure = c("HR", "F"),
  value = NULL
)
```

## Arguments

- basis_table:

  A data frame with at least one row containing columns `desc.is` and
  `desc.en` describing the prognosis basis.

- tac_hist:

  A data frame with columns `assessment_year` and `tac` giving the
  historical TAC series.

- ref_points:

  A named list with at least element `HR_mgt`.

- stock_dev:

  A long-format data frame with columns `year`, `name`, and `value`
  containing prognosis trajectories (e.g. `"ssb"` and `"ssb_ratio"`).

- assessment_year:

  Integer. The assessment year.

- fishing_pressure:

  `"HR"` (harvest rate) or `"F"`: the column of the advice basis.
  Default `"HR"`.

- value:

  The harvest rate or F of the advice. Default is `HR_mgt` or `F_mgt`
  from `ref_points`.

## Value

A one-row tibble with columns `assessment_year`, `basis.is`, `basis.en`,
`HR`, `catch`, `ssb`, `ssb_change`, `tac_current`, and `tac_previous`.
