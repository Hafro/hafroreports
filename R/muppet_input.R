#' Prepare a MUPPET option file for a given assessment year
#'
#' Takes a template MUPPET option file (as a single character string) and
#' updates the key year and age settings using \code{rmuppet::line_replace}.
#' Returns a named list suitable for passing to
#' \code{\link{hr_muppet_run}} as part of the input file set.
#'
#' @param opt_file Character string containing the full text of the MUPPET
#'   option file template. Lines are delimited by \code{"\n"}.
#' @param out_name Character. Name to use as the key in the returned list,
#'   typically the relative path to the option file within the MUPPET run
#'   directory (e.g. \code{"params/had.dat.opt"}).
#' @param year_end Integer. The last assessment year. Used to set the last
#'   data year, last optimisation year, last SMH year, and last SMB year.
#' @param age_end Integer. The last model age. Default is \code{10}.
#' @param plus_group Integer flag (0 or 1) indicating whether the last age is
#'   a plus group. Default is \code{1}.
#' @return A named list with one element: \code{out_name} mapped to the
#'   updated option file text.
#' @export
hr_muppet_input_optionfile <- function(
  opt_file,
  out_name,
  year_end,
  age_end = 10,
  plus_group = 1
) {
  out_files <- list()

  opt_file
  out_files[[out_name]] <- opt_file |>
    strsplit("\n") |>
    unlist() |>
    stringr::str_remove('../') |>
    rmuppet::line_replace(age_end, '# Last model age') |>
    rmuppet::line_replace(plus_group, '# Plus group') |>
    rmuppet::line_replace(
      year_end - 1,
      '# Last data year, last year with catch at age data'
    ) |>
    rmuppet::line_replace(
      year_end - 1,
      '# Last opt year i.e last year before assyear   <=lastdatayear'
    ) |>
    rmuppet::line_replace(year_end - 1, '# Last year smh') |>
    rmuppet::line_replace(year_end, '# Last year smb')
  return(out_files)
}
