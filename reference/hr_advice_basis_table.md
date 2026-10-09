# Format advice basis table

Creates a formatted flextable displaying the basis for catch advice,
selecting columns matching the current language setting.

## Usage

``` r
hr_advice_basis_table(basis_data, markdown = FALSE)
```

## Arguments

- basis_data:

  A data frame with columns named using language suffixes (e.g.,
  `desc.en`, `desc.is`) containing the basis text for advice.

- markdown:

  If `TRUE`, render markdown in the text (e.g. `F~MSY~` as a subscript)
  with
  [`ftExtra::colformat_md()`](https://ftextra.atusy.net/reference/colformat_md.html).
  Default `FALSE`.

## Value

A `flextable` object with two columns styled for inclusion in an advice
sheet.
