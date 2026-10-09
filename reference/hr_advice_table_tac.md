# Format TAC history table for advice sheet

Renders a formatted `flextable` showing historical advice, TAC and
landings by fishing year, with localised column headers. A note on
foreign landings before 2014 is added when those columns are shown, and
stock-specific notes can be attached to cells with `footnotes`.

## Usage

``` r
hr_advice_table_tac(
  data_tac,
  columns = c("advice_period", "advice", "tac", "icelandic", "foreign", "total"),
  footnotes = NULL,
  headers = NULL,
  foreign_footnote = TRUE
)
```

## Arguments

- data_tac:

  A data frame as returned by
  [`hr_advice_data_tac`](https://hafro.github.io/hafroreports/reference/hr_advice_data_tac.md),
  with columns `advice_period`, `advice`, `tac`, `icelandic`, `foreign`,
  and `total`.

- columns:

  Columns to show. Default all six.

- footnotes:

  List of footnotes, each a list with `i` (rows, or a function of the
  number of rows), `j` (column name or number), `en` and `is` (text),
  e.g.
  `list(list(i = 32:36, j = "advice", en = "40 % harvest control rule", is = "40 % aflaregla"))`.
  Rows outside the table are ignored. A footnote with `part = "header"`
  is attached to the column headers instead (`i` is then ignored).
  Default `NULL`.

- headers:

  Named list of column headers for columns of `data_tac` other than the
  standard six (or to replace their headers), each
  `c(en = ..., is = ...)`, e.g.
  `list(catch_14 = c(en = "Catches in East Greenland waters", is = "Afli við Austur-Grænland"))`.
  Default `NULL`.

- foreign_footnote:

  If `TRUE` (default), the note that foreign landings before 2014 are by
  calendar year (haddock) is attached to the `foreign` and `total`
  headers.

## Value

A `flextable` object styled for inclusion in an advice sheet.
