# Length frequency of the catch with the DSL reference points

The length distribution of the catch with L_c (red bar), 50\\ modal
abundance, L_inf, the largest length, the 99th percentile of the survey
lengths and L_F=M (coloured lines), as the old `R/04-DSL.R` of Norway
redfish. Labels follow `getOption("hr.lang")`.

## Usage

``` r
hr_techreport_plot_dsl_lc(basis, x_limits = c(0, 45))
```

## Arguments

- basis:

  As
  [`hr_techreport_dsl_basis`](https://hafro.github.io/hafroreports/reference/hr_techreport_dsl_basis.md).

- x_limits:

  Lengths (cm) shown. Default `c(0, 45)`.

## Value

A `ggplot2` plot object.
