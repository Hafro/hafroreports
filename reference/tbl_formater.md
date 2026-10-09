# Format a data frame as a styled GT table

Converts a data frame to a `gt` table with column header shading,
vertical column borders, missing value replacement, and
thousands-separator formatting for large numeric columns. Column names
containing `"__"` are split on that separator: the text before becomes a
spanner header and the text after becomes the column label.

## Usage

``` r
tbl_formater(x, banner_col = "#D3E7E0")
```

## Arguments

- x:

  A data frame to render as a GT table.

- banner_col:

  Character. Background colour for column headers and spanners. Default
  is `"#D3E7E0"`.

## Value

A `gt_tbl` object.
