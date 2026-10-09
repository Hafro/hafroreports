# Run the MCMC of a MUPPET model

Writes the input files to a run directory, runs MUPPET with `-mcmc` /
`-mcsave` / `-mcscale` and then `-mceval`, and reads the MCMC output
files (e.g. `resultsbyyear.mcmc`) into a long table. The option file and
output parameters must ask for the MCMC output, as in the stock's
settings.

## Usage

``` r
hr_muppet_mcmc(
  model_name,
  muppet_input_files,
  mcmc = 5e+05,
  mcsave = 1000,
  clear_on_exit = TRUE,
  md = file.path(tempdir(), paste0("muppet-mcmc-", digest::digest(muppet_input_files,
    algo = "xxh3_64"))),
  mcmc_pattern = "\\.mcmc(\\..*)?$"
)
```

## Arguments

- model_name:

  Character. A label for this model run, attached as a `model` column to
  all output tables.

- muppet_input_files:

  Named list of character strings, where each name is a file path
  relative to the run directory and each value is the file contents.
  Typically assembled from
  [`hr_muppet_input_optionfile`](https://hafro.github.io/hafroreports/reference/hr_muppet_input_optionfile.md),
  [`hr_muppet_input_datafiles`](https://hafro.github.io/hafroreports/reference/hr_muppet_input_datafiles.md),
  and
  [`hr_muppet_input_progwts`](https://hafro.github.io/hafroreports/reference/hr_muppet_input_progwts.md).

- mcmc:

  Number of MCMC iterations. Default `500000`.

- mcsave:

  Save every `mcsave`th iteration. Default `1000`.

- clear_on_exit:

  Logical. If `TRUE` (the default), the temporary run directory is
  deleted when the function exits.

- md:

  Character. Path to the run directory. Defaults to a subdirectory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) named by a hash of
  the input files.

- mcmc_pattern:

  Regular expression of the MCMC output file names; the part before it
  names the file in the result. Default: files ending in `.mcmc` or
  `.mcmc.<postfix>` (e.g. `.mcmc.base`).

## Value

A list with `mcmc_results`: a tibble with columns `iter`, `variable`,
`value`, `year` (`NA` for parameters) and `model`; and `dir`, the run
directory (kept if `clear_on_exit = FALSE`, e.g. to read other files).
