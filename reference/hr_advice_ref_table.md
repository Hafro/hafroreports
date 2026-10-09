# Format reference points table for advice sheet

Joins reference point values with their basis descriptions and renders a
formatted `flextable` with columns for approach, reference point name,
value, and basis. Column headers and cell content are localised to the
current language setting. Markdown in the basis column is rendered via
[`ftExtra::colformat_md`](https://ftextra.atusy.net/reference/colformat_md.html).

## Usage

``` r
hr_advice_ref_table(
  ref_points,
  ref_points_basis_table,
  biomass_multiplier = 1000,
  round_values = TRUE
)
```

## Arguments

- ref_points:

  A named list or one-row data frame of reference point values (e.g.
  `HR_mgt`, `B_lim`). `NA` values are dropped.

- ref_points_basis_table:

  A data frame with columns `ref_point`, `render`, `approach.en`,
  `approach.is`, `basis.en`, and `basis.is` describing each reference
  point.

- biomass_multiplier:

  Multiplier for the biomass reference points (`B_*`, `*btrigger`), to
  show values given in thousand tonnes in tonnes. Default `1000`; use
  `1` if they are already in tonnes.

- round_values:

  If `TRUE` (default), values of 1 and over are rounded to whole numbers
  (tonnes). `FALSE` shows them as they are, e.g. for relative reference
  points (F_lim = 1.7 F_MSY).

## Value

A `flextable` object styled for inclusion in an advice sheet.
