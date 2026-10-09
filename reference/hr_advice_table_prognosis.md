# Format prognosis table for advice sheet

Renders a formatted `flextable` showing the prognosis basis, catch,
harvest rate, SSB, SSB percentage change, TAC percentage change, and
advice percentage change for a given assessment year. Footnotes describe
the SSB comparison period and TAC history.

## Usage

``` r
hr_advice_table_prognosis(data_prognosis, assessment_year)
```

## Arguments

- data_prognosis:

  A data frame as returned by
  [`hr_advice_data_prognosis`](https://hafro.github.io/hafroreports/reference/hr_advice_data_prognosis.md),
  containing one or more rows with columns `assessment_year`,
  `basis.is`, `basis.en`, `catch`, `HR` or `F`, `ssb`, `ssb_change`,
  `tac_current`, and `tac_previous`.

- assessment_year:

  Integer. The assessment year row to display.

## Value

A `flextable` object styled for inclusion in an advice sheet.
