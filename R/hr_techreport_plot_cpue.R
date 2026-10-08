#' Plot CPUE time series by gear group
#'
#' Queries the \code{logbook} table, groups gears according to
#' \code{gear_group}, calculates catch per unit effort for each gear, and
#' produces a faceted CPUE time series plot.
#'
#' @param pcon A database connection object compatible with \code{dplyr::tbl}.
#' @param gear_group Named list mapping gear group labels to vectors of MFDB
#'   gear codes. Default groups are \code{GIL}, \code{BMT}, \code{LLN},
#'   and \code{DSE}.
#' @param year_start Integer. First year to include. Default is \code{1000}.
#' @param year_end Integer. Last year to include. Default is \code{9999}.
#' @return A \code{ggplot2} plot object faceted by gear group.
#' @param limit Share of the species in the catch above which records count
#'   as directed (the dashed lines), passed to
#'   \code{pax::pax_logbook_cpue_plot()}. Default \code{0.5}.
#' @param effort_na Effort of records with no tow time, hooks or nets (a
#'   bottom trawl haul without tow time), passed to
#'   \code{pax::pax_add_cpue()}. Default \code{1} (one hour); \code{NULL}
#'   leaves them out.
#' @param drop_no_effort If \code{TRUE} (default), records with none of
#'   hooks, nets or tow time are left out before \code{effort_na} applies
#'   (Danish seine records count as one set).
#' @param hooks_na Number of hooks given to records without, of any gear.
#'   Default \code{10000}, as before: a bottom trawl haul without tow time
#'   then has an effort of 10. \code{NULL} leaves them without hooks, so
#'   \code{effort_na} applies.
#' @export
hr_techreport_plot_cpue <- function(
  pcon,
  gear_group = list(
    GIL = 'GIL',
    BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
    LLN = c('HLN', 'LLN'),
    DSE = c('PSE', 'DSE')
  ),
  year_start = 1000,
  year_end = 9999,
  limit = 0.5,
  effort_na = 1,
  drop_no_effort = TRUE,
  hooks_na = 10000
) {
  # NSE variables
  year <- tow_hooks <- tow_num_nets <- tow_time <- mfdb_gear_code <- catch <- eff_miss <- NULL
  gear_name <- NULL
  coalesce <- NULL

  dat <- dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    dplyr::mutate(
      eff_miss = coalesce(
        tow_hooks,
        coalesce(tow_num_nets, coalesce(tow_time, -1))
      ),
      tow_time = ifelse(mfdb_gear_code == 'DSE', 1, tow_time),
      tow_hooks = coalesce(
        tow_hooks,
        local(if (is.null(hooks_na)) NA_real_ else hooks_na)
      ),
      tow_hooks = ifelse(tow_hooks < 1000, 1000 * tow_hooks, tow_hooks)
    )
  if (isTRUE(drop_no_effort)) {
    dat <- dplyr::filter(dat, eff_miss > -1)
  }
  dat <- dat |>
    pax::pax_add_gear_group(gear_group) |>
    dplyr::select(-mfdb_gear_code) |>
    dplyr::rename(mfdb_gear_code = gear_name) |>
    pax::pax_add_cpue(effort_na = effort_na)

  pax::pax_logbook_cpue_plot(dat, limit = limit) +
    ggplot2::labs(
      y = hr_label("catch_per_unit_effort"),
      x = hr_label("year"),
      lty = ''
    ) +
    ggplot2::facet_wrap(
      ~mfdb_gear_code,
      scales = 'free_y',
      labeller = ggplot2::labeller(
        mfdb_gear_code = c(
          `BMT` = 'Bottom Trawl',
          NPT = 'Nephrops Trawl',
          `DSE` = 'Danish Seine',
          GIL = 'Gillnet',
          LLN = 'Long Line',
          HLN = 'Handline',
          Other = 'Other'
        )
      ),
      ncol = 2
    )
}
