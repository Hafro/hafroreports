#' Combine survey indices into single input_data
#'
#' Combines commercial catch-at-age, spring (IGFS/SMB) and autumn (AGFS/SMH)
#' survey indices, and total landings into a single data frame suitable for
#' passing to \code{\link{hr_sam_dat}} or \code{\link{hr_muppet_input_datafiles}}.
#' Stock-specific rules for filling gaps and smoothing are set by the
#' arguments; with the defaults only the joins, catch scaled to landings and
#' natural mortality are applied.
#'
#' The steps, in order:
#' \enumerate{
#'   \item maturity: \code{immature_below_age} / \code{mature_above_age},
#'     then \code{maturity_fixed}, then missing values from the mean for the
#'     age (\code{maturity_fill_age_mean})
#'   \item catch and stock weights: missing values from \code{weight_fill_year},
#'     then the mean over years for the age
#'   \item catch in numbers scaled so that catch x catch weight equals the
#'     landings each year
#'   \item \code{weights_fixed} (after scaling)
#'   \item running means: stock weights of ages above
#'     \code{stock_weight_smooth_above} over \code{stock_weight_smooth_years}
#'     years, maturity over \code{maturity_smooth_years} years (current and
#'     previous years, partial windows at the start)
#'   \item natural mortality \code{M}
#' }
#'
#' @param year_start Integer. First year to include.
#' @param year_end Integer. Last year to include.
#' @param age_start Integer. Minimum age. Default is \code{0}.
#' @param age_end Integer. Maximum age. Default \code{NULL} is the oldest age
#'   in the commercial and spring survey indices; the plus group is formed
#'   later (e.g. by \code{SAMutils::sam.input()}). Cutting ages here loses the
#'   older fish and inflates younger ages when scaling to landings.
#' @param input_data_comm_index Tibble. Commercial index with columns
#'   \code{year}, \code{age}, \code{n} (catch numbers), and \code{mw}
#'   (mean weight).
#' @param input_data_igfs_index Tibble. Spring groundfish survey index with
#'   columns \code{year}, \code{age}, \code{n}, \code{mw}, and \code{mat}.
#' @param input_data_agfs_index Tibble. Autumn groundfish survey index with
#'   columns \code{year}, \code{age}, and \code{n}.
#' @param input_data_landings Tibble. Annual total landings with columns
#'   \code{year} and \code{catch}.
#' @param M Natural mortality, all ages. Default \code{0.2}.
#' @param immature_below_age,mature_above_age Ages below / above which
#'   maturity is set to 0 / 1. Default \code{NULL}.
#' @param maturity_fixed Data frame with columns \code{age}, \code{maturity}
#'   and \code{year_before}: maturity for years before \code{year_before}
#'   (e.g. before the spring survey started). Default \code{NULL}.
#' @param maturity_fill_age_mean Logical. Fill missing maturity with the mean
#'   for the age over years. Default \code{TRUE}.
#' @param maturity_mean_mature_above Ages above which fish count as mature when
#'   computing that mean. Default \code{NULL}.
#' @param weight_fill_year Year whose weights fill missing catch and stock
#'   weights for the age (before the mean over years). Default \code{NULL}.
#' @param catch_weight_fill_year,stock_weight_fill_year The same for catch
#'   and stock weights separately. Default \code{weight_fill_year}.
#' @param catch_weight_fill_mean,stock_weight_fill_mean Fill (remaining)
#'   missing catch / stock weights with the mean over years for the age.
#'   \code{FALSE} leaves them missing (e.g. for SAM to fill). Default
#'   \code{TRUE}.
#' @param stock_survey Survey index giving the stock weights and maturity:
#'   \code{"igfs"} (spring, default) or \code{"agfs"} (autumn; then
#'   \code{input_data_agfs_index} needs \code{mw} and \code{mat}).
#'   The other index gives only its numbers.
#' @param weights_fixed Data frame with columns \code{age},
#'   \code{catch_weight} and \code{stock_weight} (g), applied after scaling to
#'   landings. Default \code{NULL}.
#' @param stock_weight_smooth_above,stock_weight_smooth_years Running mean of
#'   stock weights for ages above \code{stock_weight_smooth_above}. Default
#'   \code{NULL}.
#' @param maturity_smooth_years Running mean of maturity. Default \code{NULL}.
#' @return A tibble with columns \code{year}, \code{age}, \code{catch},
#'   \code{catch_weight}, \code{smb}, \code{stock_weight}, \code{maturity},
#'   \code{smh}, and \code{M}, covering the requested year and age range.
#' @export
hr_input_data_combine <- function(
  year_start,
  year_end,
  age_start = 0,
  age_end = NULL,
  input_data_comm_index,
  input_data_igfs_index,
  input_data_agfs_index,
  input_data_landings,
  M = 0.2,
  immature_below_age = NULL,
  mature_above_age = NULL,
  maturity_fixed = NULL,
  maturity_fill_age_mean = TRUE,
  maturity_mean_mature_above = NULL,
  weight_fill_year = NULL,
  weights_fixed = NULL,
  stock_weight_smooth_above = NULL,
  stock_weight_smooth_years = NULL,
  maturity_smooth_years = NULL,
  catch_weight_fill_year = weight_fill_year,
  stock_weight_fill_year = weight_fill_year,
  catch_weight_fill_mean = TRUE,
  stock_weight_fill_mean = TRUE,
  stock_survey = c("igfs", "agfs")
) {
  stock_survey <- match.arg(stock_survey)
  # NSE variables
  n <- mw <- mat <- lnd <- mat_mean <- year_before <- maturity_fixed_value <- NULL
  year <- age <- catch <- catch_weight <- stock_weight <- maturity <- NULL
  catch_weight_fixed <- stock_weight_fixed <- NULL

  # Running mean over the current and up to k - 1 previous years
  run_mean <- function(x, k) {
    vapply(
      seq_along(x),
      function(i) mean(x[max(1, i - k + 1):i], na.rm = TRUE),
      numeric(1)
    )
  }
  fill_weight <- function(w, year, fill_year, fill_mean) {
    if (!is.null(fill_year)) {
      w <- dplyr::coalesce(w, w[year == fill_year][1])
    }
    if (isTRUE(fill_mean)) {
      w <- dplyr::coalesce(w, mean(w, na.rm = TRUE))
    }
    w
  }

  input_data_comm_index <- dplyr::collect(input_data_comm_index)
  input_data_igfs_index <- dplyr::collect(input_data_igfs_index)
  input_data_agfs_index <- dplyr::collect(input_data_agfs_index)
  # The survey giving stock weights and maturity
  stock_index <- if (stock_survey == "igfs") {
    input_data_igfs_index
  } else {
    input_data_agfs_index
  }
  if (is.null(age_end)) {
    age_end <- max(input_data_comm_index$age, input_data_igfs_index$age)
  }
  mean_mat <- stock_index |>
    dplyr::mutate(
      mat = if (is.null(maturity_mean_mature_above)) {
        mat
      } else {
        ifelse(age > maturity_mean_mature_above, 1, mat)
      }
    ) |>
    dplyr::group_by(age) |>
    dplyr::summarise(mat_mean = mean(mat, na.rm = TRUE))

  out <- tidyr::expand_grid(
    year = year_start:year_end,
    age = age_start:age_end
  ) |>
    dplyr::left_join(
      input_data_comm_index |>
        dplyr::select(year, age, catch = n, catch_weight = mw),
      by = c('year', 'age')
    ) |>
    dplyr::left_join(
      input_data_igfs_index |> dplyr::select(year, age, smb = n),
      by = c('year', 'age')
    ) |>
    dplyr::left_join(
      stock_index |> dplyr::select(year, age, stock_weight = mw, maturity = mat),
      by = c('year', 'age')
    ) |>
    dplyr::left_join(
      input_data_agfs_index |> dplyr::select(year, age, smh = n),
      by = c('year', 'age')
    ) |>
    dplyr::left_join(
      input_data_landings |> dplyr::collect() |> dplyr::rename(lnd = catch),
      by = 'year'
    )

  # Maturity
  if (!is.null(immature_below_age)) {
    out <- dplyr::mutate(out, maturity = ifelse(age < immature_below_age, 0, maturity))
  }
  if (!is.null(mature_above_age)) {
    out <- dplyr::mutate(out, maturity = ifelse(age > mature_above_age, 1, maturity))
  }
  if (!is.null(maturity_fixed)) {
    out <- out |>
      dplyr::left_join(
        maturity_fixed |>
          dplyr::select(age, year_before, maturity_fixed_value = maturity),
        by = 'age'
      ) |>
      dplyr::mutate(
        maturity = ifelse(
          !is.na(year_before) & year < year_before,
          maturity_fixed_value,
          maturity
        )
      ) |>
      dplyr::select(-year_before, -maturity_fixed_value)
  }
  if (isTRUE(maturity_fill_age_mean)) {
    out <- out |>
      dplyr::left_join(mean_mat, by = 'age') |>
      dplyr::mutate(maturity = dplyr::coalesce(maturity, mat_mean)) |>
      dplyr::select(-mat_mean)
  }

  # Weights, catch scaled to landings
  out <- out |>
    dplyr::group_by(age) |>
    dplyr::mutate(
      catch_weight = fill_weight(
        catch_weight,
        year,
        catch_weight_fill_year,
        catch_weight_fill_mean
      ),
      stock_weight = fill_weight(
        stock_weight,
        year,
        stock_weight_fill_year,
        stock_weight_fill_mean
      ),
      catch = tidyr::replace_na(catch, 0)
    ) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      catch = catch * lnd / sum(catch * catch_weight, na.rm = TRUE)
    ) |>
    dplyr::ungroup()
  if (!is.null(weights_fixed)) {
    out <- out |>
      dplyr::left_join(
        weights_fixed |>
          dplyr::select(
            age,
            catch_weight_fixed = catch_weight,
            stock_weight_fixed = stock_weight
          ),
        by = 'age'
      ) |>
      dplyr::mutate(
        catch_weight = dplyr::coalesce(catch_weight_fixed, catch_weight),
        stock_weight = dplyr::coalesce(stock_weight_fixed, stock_weight),
        catch = tidyr::replace_na(catch, 0)
      ) |>
      dplyr::select(-catch_weight_fixed, -stock_weight_fixed)
  }

  # Smoothing
  out <- out |>
    dplyr::arrange(year, age) |>
    dplyr::group_by(age)
  if (!is.null(stock_weight_smooth_above)) {
    out <- dplyr::mutate(
      out,
      stock_weight = ifelse(
        age > stock_weight_smooth_above,
        run_mean(stock_weight, stock_weight_smooth_years),
        stock_weight
      )
    )
  }
  if (!is.null(maturity_smooth_years)) {
    out <- dplyr::mutate(out, maturity = run_mean(maturity, maturity_smooth_years))
  }
  out |>
    dplyr::ungroup() |>
    dplyr::select(-lnd) |>
    dplyr::mutate(M = M)
}
