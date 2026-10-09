# Run MUPPET model and return results

Writes a named list of input files to a temporary directory, locates the
option file (`params/*.dat.opt`), executes MUPPET via
[`rmuppet::callMuppet`](https://rdrr.io/pkg/rmuppet/man/callMuppet.html),
and reads back the output tables.

## Usage

``` r
hr_muppet_run(
  model_name,
  muppet_input_files,
  clear_on_exit = TRUE,
  md = file.path(tempdir(), paste0("muppet-run-", digest::digest(muppet_input_files,
    algo = "xxh3_64"))),
  muppet_args = c("nox")
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

- clear_on_exit:

  Logical. If `TRUE` (the default), the temporary run directory is
  deleted when the function exits.

- md:

  Character. Path to the run directory. Defaults to a subdirectory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) named by a hash of
  the input files.

- muppet_args:

  Character vector of additional arguments passed to
  [`rmuppet::callMuppet`](https://rdrr.io/pkg/rmuppet/man/callMuppet.html).
  Default is `c("nox")`.

## Value

A named list with some or all of the following elements, depending on
which output files are produced:

- `rby`:

  Results by year from `resultsbyyear.out`.

- `rbyage`:

  Results by year and age from `resultsbyyearandage.out`.

- `params`:

  Parameter estimates from `muppet.std`, with log-scale parameters
  back-transformed.
