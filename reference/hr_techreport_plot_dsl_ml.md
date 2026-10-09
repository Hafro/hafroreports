# Mean length of the catch by year with L_F=M

The mean length above L_c of the catch by year, with L_F=M (dashed), as
the old `R/04-DSL.R` of Norway redfish. Labels follow
`getOption("hr.lang")`.

## Usage

``` r
hr_techreport_plot_dsl_ml(
  ldist,
  basis,
  year_start,
  year_end,
  y_limits = NULL,
  label_x = year_end - 9
)
```

## Arguments

- ldist:

  Length distributions of the catch by year (columns `year`, `length`,
  `n`).

- basis:

  As
  [`hr_techreport_dsl_basis`](https://hafro.github.io/hafroreports/reference/hr_techreport_dsl_basis.md)
  (`Lc`, `LF`).

- year_start, year_end:

  Years on the x axis.

- y_limits:

  Fixed y axis `c(lower, upper)` (cm, breaks every cm), or `NULL`
  (default) for the default axis.

- label_x:

  Year of the L_F=M label. Default `year_end - 9`.

## Value

A `ggplot2` plot object.
