# Format the constant harvest rate (chr) advice calculation table

Table of the category 3 constant harvest rate rule (ICES method 2.2):
the previous advice, the latest index, the MSY proxy harvest rate, the
biomass safeguard, the precautionary multiplier, the stability clause
and the advice. As `tidypax:::chr_prognosis_table()`, from the output of
`dlsrules::fproxy_rule()` instead of a file.

## Usage

``` r
hr_advice_table_chr(
  chr_prognosis,
  assessment_year,
  chr_prognosis_base = readr::read_csv(system.file("extdata", "chr_prognosis_base.csv",
    package = "hafroreports"), show_col_types = FALSE)
)
```

## Arguments

- chr_prognosis:

  Data frame with columns `component` and `value` (the output of
  `dlsrules::fproxy_rule()`, component names in lower or original case).

- assessment_year:

  Integer. The assessment year, used in the row descriptions.

- chr_prognosis_base:

  Data frame with the row layout: `label`, `chr_desc.en`, `chr_desc.is`
  and `component` (`NA` for heading rows). `{tyr}` in the descriptions
  is replaced by the assessment year. Default: the tidypax layout,
  bundled with the package.

## Value

A `flextable` object styled for inclusion in an advice sheet.
