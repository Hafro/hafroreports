#' Merge and update a historical data series
#'
#' Combines one or more data frames (or CSV file paths) into a single
#' historical table. When a new data frame contains rows for an
#' \code{assessment_year} already present in the accumulator, the old rows
#' are replaced. \code{NULL} arguments are silently skipped. Template rows
#' (where \code{assessment_year} is \code{NA}) are removed from the output.
#'
#' This is useful for maintaining a rolling series across assessment years,
#' e.g. storing advice or TAC history incrementally.
#'
#' @param ... Data frames or character paths to CSV files, each with an
#'   \code{assessment_year} column. Later arguments take precedence over
#'   earlier ones for the same \code{assessment_year} values.
#' @return A tibble containing all rows from the merged inputs, with
#'   duplicate \code{assessment_year} entries resolved in favour of the
#'   latest argument, and template rows (\code{assessment_year = NA}) removed.
#' @export
hr_update_hist <- function(
  ...
) {
  # NSE variables
  assessment_year <- NULL

  out <- NULL
  for (a in list(...)) {
    if (is.null(a)) {
      next
    }
    if (is.character(a) && endsWith(a, ".csv")) {
      a <- readr::read_csv(a)
    }

    # Combine with previous data
    if (is.null(out)) {
      out <- a
    } else {
      out <- out |>
        # Remove any rows for this assessment already in data
        dplyr::filter(!(assessment_year %in% unique(a$assessment_year))) |>
        dplyr::bind_rows(a)
    }
  }
  # Remove any template rows once done
  out <- dplyr::filter(out, !is.na(assessment_year))

  return(out)
}
