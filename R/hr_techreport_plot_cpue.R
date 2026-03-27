hr_techreport_plot_cpue <- function(
  pcon,
  gear_group = list(
    GIL = 'GIL',
    BMT = c('BMT', 'NPT', 'SHT', 'PGT', 'DRD'),
    LLN = c('HLN', 'LLN'),
    DSE = c('PSE', 'DSE')
  ),
  year_start = 1000,
  year_end = 9999
) {
  dat <- dplyr::tbl(pcon, "logbook") |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    dplyr::mutate(
      eff_miss = coalesce(
        tow_hooks,
        coalesce(tow_num_nets, coalesce(tow_time, -1))
      ),
      tow_time = ifelse(mfdb_gear_code == 'DSE', 1, tow_time),
      tow_hooks = coalesce(tow_hooks, 10000),
      tow_hooks = ifelse(tow_hooks < 1000, 1000 * tow_hooks, tow_hooks)
    ) |>
    dplyr::filter(eff_miss > -1) |>
    pax::pax_add_gear_group(gear_group) |>
    dplyr::select(-mfdb_gear_code) |>
    dplyr::rename(mfdb_gear_code = gear_name) |>
    pax::pax_add_cpue()

  pax::pax_logbook_cpue_plot(dat) +
    ggplot2::facet_wrap(
      ~mfdb_gear_code,
      scales = 'free_y',
      labeller = ggplot2::labeller(
        mfdb_gear_code = c(
          `BMT` = 'Bottom Trawl',
          `DSE` = 'Danish Seine',
          GIL = 'Gillnet',
          LLN = 'Long Line'
        )
      ),
      ncol = 2
    )
}
