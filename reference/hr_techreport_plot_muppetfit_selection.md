# Plot MUPPET model selection and fit diagnostics

Produces a four-panel diagnostic plot for a MUPPET `logit_length` model
fit:

1.  Selectivity-at-weight curve with MCMC uncertainty ribbon.

2.  Age-specific observation standard deviations for catch and surveys.

3.  Stock–recruitment scatter plot.

4.  Catchability-at-age (\\q\\) for spring and autumn surveys.

## Usage

``` r
hr_techreport_plot_muppetfit_selection(fit, assessment_year)
```

## Arguments

- fit:

  A list returned by
  [`hr_muppet_run`](https://hafro.github.io/hafroreports/reference/hr_muppet_run.md),
  containing elements `$params`, `$rby`, `$rbage`, and `$mcmc_results`.

- assessment_year:

  Integer. Used to restrict the stock–recruitment scatter to years
  before the assessment year.

## Value

A `patchwork` plot object with four panels.
