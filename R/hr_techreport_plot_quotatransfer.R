# Was transfer_plot
hr_techreport_plot_quotatransfer <- function(pcon, assessment_year) {
  dat <- dplyr::tbl(pcon, "quotatransfer") |>
    dplyr::filter(as.numeric(substr(fishing_year, 1, 4)) < assessment_year)

  # NSE variables
  m_ara <- NULL
  n_ar <- NULL
  onotad <- NULL
  varanlegt <- NULL
  tilf <- NULL
  fishing_year <- NULL
  m_p <- NULL
  til_p <- NULL
  value <- NULL

  dat |>
    dplyr::ungroup() |>
    dplyr::mutate(
      #m_ara = n_ar,
      m_p = 100 * (m_ara - n_ar - onotad) / varanlegt,
      til_p = 100 * tilf / varanlegt,
      m_ara = (m_ara - n_ar - onotad) / 1e3,
      tilf = tilf / 1e3
    ) |>
    dplyr::select(fishing_year, tilf, m_ara, m_p, til_p) |>
    dplyr::collect() |>
    tidyr::gather(col, value, -fishing_year) |>
    dplyr::mutate(
      col = ifelse(
        col == 'm_ara',
        local(hr_label("between_years")),
        ifelse(
          col == 'm_p',
          local(paste0(hr_label("between_years"), " (%)")),
          ifelse(
            col == 'tilf',
            local(hr_label("between_species")),
            local(paste0(hr_label("between_species"), " (%)"))
          )
        )
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(fishing_year, value)) +
    ggplot2::geom_bar(stat = 'identity') +
    ggplot2::facet_wrap(~col, ncol = 2, scale = 'free_y') +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = hr_label("quota_period"),
      y = hr_label("transfers_in_thous_t")
    )
}
