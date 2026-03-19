# hafroreports: Helpers for building assessment reports

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
