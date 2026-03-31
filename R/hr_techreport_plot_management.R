# Was management_plot
#' Plot management history (advice, TAC, and landings)
#'
#' Combines historical advice, TAC, and total landings into a single line
#' plot by fishing year. User-supplied annotations (e.g. regulation changes)
#' can be added via \code{m_labels}.
#'
#' @param advice_hist A data frame with columns \code{assessment_year},
#'   \code{advice}, and \code{advice_period}.
#' @param tac_hist A data frame with columns \code{assessment_year} and
#'   \code{tac}.
#' @param landings_by_fishing_year_country A data frame with columns
#'   \code{fishing_year}, \code{country}, and \code{catch} (in tonnes).
#' @param m_labels A data frame with columns \code{Year}, \code{value}
#'   (in tonnes), and \code{type} (label text) for annotations placed on the
#'   plot.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_management <- function(
  advice_hist,
  tac_hist,
  landings_by_fishing_year_country,
  m_labels
) {
  # NSE variables
  assessment_year <- advice_period <- advice <- tac <- total <- fishing_year <- NULL
  value <- group <- Year <- type <- catch <- NULL

  dat <- advice_hist |>
    dplyr::inner_join(tac_hist, by = "assessment_year") |>
    dplyr::inner_join(
      landings_by_fishing_year_country |>
        dplyr::group_by(fishing_year) |>
        dplyr::summarize(total = sum(catch) / 1e3),
      ,
      by = c("advice_period" = "fishing_year")
    ) |>
    dplyr::select(advice_period, advice, tac, total) |>
    tidyr::pivot_longer(
      -advice_period,
      values_to = 'value',
      names_to = 'group'
    ) |>
    dplyr::mutate(
      group = dplyr::case_when(
        group == 'tac' ~ 'Total allowable catch',
        group == 'total' ~ 'Total landings',
        TRUE ~ 'Adviced catch'
      )
    )

  ggplot2::ggplot(dat) +
    ggplot2::geom_line(ggplot2::aes(
      advice_period,
      value / 1e3,
      col = group,
      group = group
    )) +
    ggplot2::expand_limits(y = 0) +
    ggrepel::geom_label_repel(
      data = m_labels,
      ggplot2::aes(Year, value / 1e3, label = type),
      arrow = ggplot2::arrow(length = ggplot2::unit(0.02, "npc")),
      force = 10,
      ylim = c(NA, 25)
    ) +
    ggplot2::labs(col = '', x = '', y = 'Landings (in kt)') +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      legend.position = c(0.2, 0.8),
      legend.background = ggplot2::element_blank()
    ) +
    ggplot2::scale_color_manual(values = c('red', 'blue', 'black')) +
    ggplot2::expand_limits(y = 0)
}
