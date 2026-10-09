# Length-based reference point basis of a DSL figure

L_c (the first length with more than 50\\ catch), the largest length and
the 99th percentile of the survey lengths, L_inf (their mean) and L_F=M
= 0.75 L_c + 0.25 L_inf, as the old `R/04-DSL.R` of Norway redfish; the
input of
[`hr_techreport_plot_dsl_lc`](https://hafro.github.io/hafroreports/reference/hr_techreport_plot_dsl_lc.md)
and
[`hr_techreport_plot_dsl_ml`](https://hafro.github.io/hafroreports/reference/hr_techreport_plot_dsl_ml.md).

## Usage

``` r
hr_techreport_dsl_basis(ldist, survey_ldist)
```

## Arguments

- ldist:

  Length distributions of the catch (columns `length`, `n`; summed over
  years).

- survey_ldist:

  Length distributions of the survey (columns `length`, `n`).

## Value

A list with `ldist_all` (the catch by length), `modal_abun50`, `Lc`,
`q99`, `max_len`, `Linf` and `LF`.
