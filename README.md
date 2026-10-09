# hafroreports: Helpers for building assessment reports

<!-- badges: start -->
[![R-CMD-check](https://github.com/hafro/hafroreports/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hafro/hafroreports/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/hafro/hafroreports/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/hafro/hafroreports/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

Figures, tables and input data for the MFRI stock assessment reports, used by
the stock assessment repositories (01-cod, 02-had-targets, 03-sai, ...).

Documentation, with articles on the main workflows, is at
<https://hafro.github.io/hafroreports/>:

* [Input data for SAM and Muppet](https://hafro.github.io/hafroreports/articles/input-data.html)
* [History tables](https://hafro.github.io/hafroreports/articles/history-tables.html)
* [Advice sheet figures and tables](https://hafro.github.io/hafroreports/articles/advice-sheets.html)
* [Tech report figures](https://hafro.github.io/hafroreports/articles/techreport-figures.html)
* [The layout of a stock repository](https://hafro.github.io/hafroreports/articles/stock-repository.html)

The badges and the site work once the workflows have run on GitHub and
GitHub Pages is enabled for the `gh-pages` branch.

## Installation

```r
remotes::install_github("hafro/hafroreports")
```

SAMutils, stockassessment and rmuppet (in Enhances) are needed only for the
SAM and Muppet functions.

## Development

### Code formatting

This projects uses [Air](https://posit-dev.github.io/air/), you may need to configure your editor accordingly.
See https://posit-dev.github.io/air/editors.html

### Testing within an assessment model

Assessment models use [renv](https://rstudio.github.io/renv/articles/renv.html), and your development version will need to be installed before you can test.

```{r}
renv::install("local::../hafroreports", dependencies = "never")
```

...once your changes are pushed / merged, switch back to the github version with:


```{r}
renv::install("github::hafro/hafroreports")
renv::snapshot()
```
