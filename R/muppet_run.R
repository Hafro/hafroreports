# Write (fl), a list of file path->plain-text contents to (workdir)
write_file_list <- function(fl, workdir) {
  for (f_name in names(fl)) {
    f_path <- file.path(workdir, f_name)
    dir.create(dirname(f_path), showWarnings = FALSE, recursive = TRUE)
    writeLines(fl[[f_name]], con = f_path)
  }
  return(workdir)
}

#' Run MUPPET model and return results
#'
#' Writes a named list of input files to a temporary directory, locates the
#' option file (\code{params/*.dat.opt}), executes MUPPET via
#' \code{rmuppet::callMuppet}, and reads back the output tables.
#'
#' @param model_name Character. A label for this model run, attached as a
#'   \code{model} column to all output tables.
#' @param muppet_input_files Named list of character strings, where each name
#'   is a file path relative to the run directory and each value is the file
#'   contents. Typically assembled from \code{\link{hr_muppet_input_optionfile}},
#'   \code{\link{hr_muppet_input_datafiles}}, and
#'   \code{\link{hr_muppet_input_progwts}}.
#' @param clear_on_exit Logical. If \code{TRUE} (the default), the temporary
#'   run directory is deleted when the function exits.
#' @param md Character. Path to the run directory. Defaults to a subdirectory
#'   of \code{tempdir()} named by a hash of the input files.
#' @param muppet_args Character vector of additional arguments passed to
#'   \code{rmuppet::callMuppet}. Default is \code{c("nox")}.
#' @return A named list with some or all of the following elements, depending
#'   on which output files are produced:
#'   \describe{
#'     \item{\code{rby}}{Results by year from \code{resultsbyyear.out}.}
#'     \item{\code{rbyage}}{Results by year and age from
#'       \code{resultsbyyearandage.out}.}
#'     \item{\code{params}}{Parameter estimates from \code{muppet.std}, with
#'       log-scale parameters back-transformed.}
#'   }
#' @export
hr_muppet_run <- function(
  model_name,
  muppet_input_files,
  clear_on_exit = TRUE,
  md = file.path(
    tempdir(),
    paste0("muppet-run-", digest::digest(muppet_input_files, algo = "xxh3_64"))
  ),
  muppet_args = c('nox')
) {
  # NSE variables
  CalcCno <- name <- value <- variable <- NULL

  write_file_list(muppet_input_files, md)
  if (isTRUE(clear_on_exit)) {
    on.exit(unlink(md, recursive = TRUE), add = TRUE)
  }

  ind <- grep(
    '^params/.*\\.dat\\.opt$',
    names(muppet_input_files),
    value = TRUE
  )
  if (length(ind) != 1) {
    stop("Zero (or multiple) option files found at params/*.dat.opt")
  }
  res <- withr::with_dir(md, rmuppet::callMuppet(c(ind = ind, muppet_args)))
  if (!is.null(attr(res, "status"))) {
    writeLines(res)
    stop("rmuppet failed, returning status ", attr(res, "status"))
  }

  read_output <- function(path) {
    readr::read_table(path) |>
      dplyr::mutate_all(function(x) ifelse(x == -1, NA, x)) |>
      dplyr::mutate(model = local(model_name))
  }

  fit <- list()
  if (file.exists(file.path(md, "resultsbyyear.out"))) {
    fit$rby <- read_output(file.path(md, "resultsbyyear.out"))
  }
  if (file.exists(file.path(md, "resultsbyyearandage.out"))) {
    fit$rbyage <- read_output(file.path(md, "resultsbyyearandage.out")) |>
      dplyr::mutate(CalcCno = ifelse(model_name == 'vpa', NA, CalcCno))
  }
  if (file.exists(file.path(md, "muppet.std"))) {
    fit$params <- read_output(file.path(md, "muppet.std")) |>
      dplyr::mutate(
        value = ifelse(
          grepl('ln|log|estSSBRecParameters', name),
          exp(value),
          value
        ),
        variable = gsub('ln|log', '', name) |>
          gsub('([a-zA-Z]+)\\[([0-9])\\]', '\\1.\\2', x = _),
        variable = ifelse(
          grepl('estSSBRecParameters', variable),
          forcats::fct_recode(
            gsub('estSSBRecParameters.', '', variable, fixed = TRUE),
            Rmax = "1",
            ssbbreak = "2",
            `Recruitment CV` = '3',
            rho = '4'
          ) |>
            as.character(),
          variable
        )
      )
  }

  return(fit)
}

#' Run the MCMC of a MUPPET model
#'
#' Writes the input files to a run directory, runs MUPPET with
#' \code{-mcmc} / \code{-mcsave} / \code{-mcscale} and then \code{-mceval},
#' and reads the MCMC output files (e.g. \code{resultsbyyear.mcmc}) into a
#' long table. The option file and output parameters must ask for the MCMC
#' output, as in the stock's settings.
#'
#' @inheritParams hr_muppet_run
#' @param mcmc Number of MCMC iterations. Default \code{500000}.
#' @param mcsave Save every \code{mcsave}th iteration. Default \code{1000}.
#' @param mcmc_pattern Regular expression of the MCMC output file names;
#'   the part before it names the file in the result. Default: files ending
#'   in \code{.mcmc} or \code{.mcmc.<postfix>} (e.g. \code{.mcmc.base}).
#' @return A list with \code{mcmc_results}: a tibble with columns
#'   \code{iter}, \code{variable}, \code{value}, \code{year} (\code{NA}
#'   for parameters) and \code{model}; and \code{dir}, the run directory
#'   (kept if \code{clear_on_exit = FALSE}, e.g. to read other files).
#' @export
hr_muppet_mcmc <- function(
  model_name,
  muppet_input_files,
  mcmc = 500000,
  mcsave = 1000,
  clear_on_exit = TRUE,
  md = file.path(
    tempdir(),
    paste0("muppet-mcmc-", digest::digest(muppet_input_files, algo = "xxh3_64"))
  ),
  mcmc_pattern = "\\.mcmc(\\..*)?$"
) {
  # NSE variables
  iter <- filename <- variable <- value <- year <- NULL

  write_file_list(muppet_input_files, md)
  if (isTRUE(clear_on_exit)) {
    on.exit(unlink(md, recursive = TRUE), add = TRUE)
  }
  ind <- grep(
    '^params/.*\\.dat\\.opt$',
    names(muppet_input_files),
    value = TRUE
  )
  if (length(ind) != 1) {
    stop("Zero (or multiple) option files found at params/*.dat.opt")
  }
  withr::with_dir(
    md,
    rmuppet::callMuppet(c(
      ind = ind,
      'nox',
      mcmc = mcmc,
      mcsave = mcsave,
      'mcscale'
    ))
  )
  res <- withr::with_dir(md, rmuppet::callMuppet(c(ind = ind, 'mceval')))
  if (!is.null(attr(res, "status"))) {
    writeLines(res)
    stop("rmuppet -mceval failed, returning status ", attr(res, "status"))
  }

  mc_files <- list.files(md, pattern = mcmc_pattern, full.names = TRUE)
  mc_files <- mc_files[basename(mc_files) != "all.mcmc"]
  mcmc_results <- mc_files |>
    stats::setNames(sub(mcmc_pattern, "", basename(mc_files))) |>
    as.list() |>
    purrr::map(readr::read_table, show_col_types = FALSE) |>
    purrr::map(dplyr::mutate, iter = 1:dplyr::n()) |>
    purrr::map(
      tidyr::pivot_longer,
      -iter,
      names_to = 'variable',
      values_to = 'value',
      values_transform = list(value = as.numeric)
    ) |>
    dplyr::bind_rows(.id = 'filename') |>
    dplyr::mutate(
      year = ifelse(
        grepl('parameter', filename),
        NA,
        stringr::str_extract(variable, '\\.([0-9]+)')
      ),
      year = as.numeric(gsub('\\.', '', year)),
      variable = ifelse(
        grepl('parameter', filename),
        variable,
        stringr::str_remove(variable, '\\.[0-9]+')
      ),
      value = ifelse(
        grepl('estSSBRecParameters', variable),
        exp(value),
        value
      ),
      variable = ifelse(
        grepl('estSSBRecParameters', variable),
        forcats::fct_recode(
          gsub('estSSBRecParameters.', '', variable, fixed = TRUE),
          Rmax = "1",
          ssbbreak = "2",
          `Recruitment CV` = '3',
          rho = '4'
        ) |>
          as.character(),
        variable
      ),
      model = model_name
    ) |>
    dplyr::select(-filename)

  list(mcmc_results = mcmc_results, dir = md)
}
