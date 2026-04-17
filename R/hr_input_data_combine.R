#' Combine survey indices into single input_data
#'
#' Combines commercial catch-at-age, spring (IGFS/SMB) and autumn (AGFS/SMH)
#' survey indices, and total landings into a single data frame suitable for
#' passing to \code{\link{hr_sam_dat}} or \code{\link{hr_muppet_input_datafiles}}.
#' Hard-coded stock weights and maturity ogives (for ages 1–14) are used to
#' fill gaps before 1985 or when survey data are unavailable.
#'
#' @param year_start Integer. First year to include.
#' @param year_end Integer. Last year to include.
#' @param age_start Integer. Minimum age. Default is \code{1}.
#' @param age_end Integer. Maximum age; defaults to the maximum age in
#'   \code{input_data_comm_index}.
#' @param input_data_comm_index Tibble. Commercial survey index with columns
#'   \code{year}, \code{age}, \code{n} (catch numbers), and \code{mw}
#'   (mean weight).
#' @param input_data_igfs_index Tibble. Spring groundfish survey index with
#'   columns \code{year}, \code{age}, \code{n}, \code{mw}, and \code{mat}.
#' @param input_data_agfs_index Tibble. Autumn groundfish survey index with
#'   columns \code{year}, \code{age}, and \code{n}.
#' @param input_data_landings Tibble. Annual total landings with columns
#'   \code{year} and \code{catch}.
#' @return A tibble with columns \code{year}, \code{age}, \code{catch},
#'   \code{catch_weight}, \code{smb}, \code{stock_weight}, \code{maturity},
#'   \code{smh}, and \code{M}, covering the requested year and age range.
#' @export
hr_input_data_combine <- function(
  year_start,
  year_end,
  age_start = 1,
  age_end = max(input_data_comm_index$age),
  input_data_comm_index,
  input_data_igfs_index,
  input_data_agfs_index,
  input_data_landings
) {
  # NSE variables
  n <- mw <- mat <- smb <- lnd <- ws <- ms <- NULL
  year <- age <- catch <- catch_weight <- stock_weight <- maturity <- NULL
  tidyr::expand_grid(
    year = year_start:year_end,
    age = age_start:age_end
  ) |>
    dplyr::full_join(input_data_comm_index) |>
    dplyr::rename(catch = n, catch_weight = mw) |>
    dplyr::full_join(
      input_data_igfs_index |>
        dplyr::rename(smb = n, stock_weight = mw, maturity = mat),
      by = c('year', 'age')
    ) |>
    dplyr::full_join(
      input_data_agfs_index |> dplyr::select(year, age, smh = n),
      by = c('year', 'age')
    ) |>
    dplyr::left_join(
      input_data_landings |> dplyr::rename(lnd = catch),
      by = c('year')
    ) |>
    dplyr::left_join(tibble::tibble(
      age = 1:14,
      ws = c(
        37,
        185,
        481,
        910,
        1409,
        1968,
        2496,
        3077,
        3300,
        4000,
        5741,
        6171,
        4000,
        5213
      )
    )) |>
    dplyr::left_join(tibble::tibble(
      age = 1:14,
      ms = c(
        0,
        0.08,
        0.301,
        0.539,
        0.722,
        0.821,
        0.868,
        0.904,
        0.963,
        1,
        1,
        1,
        1,
        1
      )
    )) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      catch = catch * lnd / sum(catch * catch_weight, na.rm = TRUE),
      stock_weight = ifelse(
        is.na(stock_weight) & year < 1985,
        ws,
        stock_weight
      ),
      maturity = ifelse(is.na(maturity), ms, maturity)
    ) |>
    dplyr::mutate(
      catch_weight = tidyr::replace_na(catch_weight, 4000),
      stock_weight = tidyr::replace_na(stock_weight, 4000),
      catch = tidyr::replace_na(catch, 0)
    ) |>
    dplyr::select(-c(lnd, ms, ws)) |>
    dplyr::arrange(year, age) |>
    dplyr::mutate(M = 0.2)
}
