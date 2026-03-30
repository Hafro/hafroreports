hr_update_hist <- function(
  ...
) {
  out <- NULL
  for (a in list(...)) {
    if (is.null(a)) {
      continue
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
