# Format the rfb (ratio, f, b) advice calculation table

Table of the category 3 rfb rule (ICES method 2.1): the previous advice,
the index ratio, the fishing pressure proxy from mean catch length, the
biomass safeguard, the precautionary multiplier, the stability clause
and the advice. As `tidypax:::rfb_prognosis_table()`, from the output of
`dlsrules::rfb_rule()` instead of a file.

## Usage

``` r
hr_advice_table_rfb(
  rfb_prognosis,
  assessment_year,
  biannual = TRUE,
  rfb_prognosis_base = readr::read_csv(system.file("extdata", "rfb_prognosis_base.csv",
    package = "hafroreports"), show_col_types = FALSE)
)
```

## Arguments

- rfb_prognosis:

  Data frame with columns `component` and `value` (the output of
  `dlsrules::rfb_rule()`).

- assessment_year:

  Integer. The assessment year, used in the row descriptions.

- biannual:

  `TRUE` (default) if the advice is for two fishing years, as the rfb
  rule usually is.

- rfb_prognosis_base:

  Data frame with the row layout: `label`, `rfb_desc.en`, `rfb_desc.is`
  and `component`. Default: the tidypax layout, bundled with the
  package.

## Value

A `flextable` object styled for inclusion in an advice sheet.
