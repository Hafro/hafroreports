hr_techreport_plot_discards <- function(
  species
) {
  hafroreports::hr_discards |>
    dplyr::filter(species == .env$species) |>
    dplyr::bind_rows(
      hafroreports::hr_discards |>
        dplyr::filter(species == .env$species) |>
        dplyr::mutate(tot = disc.wgt / disc.wgt.perc) |>
        dplyr::group_by(year) |>
        dplyr::summarise(
          gear = ' Total from all gears',
          disc.wgt.perc = sum(tot * disc.wgt.perc, na.rm = TRUE) /
            sum(tot, na.rm = TRUE),
          disc.wgt = sum(disc.wgt, na.rm = TRUE),
          CV = sum(tot * CV, na.rm = TRUE) / sum(tot, na.rm = TRUE)
        )
    ) |>
    dplyr::mutate(
      u = disc.wgt.perc * exp(1.96 * CV / 100),
      l = disc.wgt.perc * exp(-1.96 * CV / 100),
    ) |>
    ggplot2::ggplot(ggplot2::aes(year, disc.wgt.perc)) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = l, ymax = u)) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~gear) +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = 'Discards (% weight)', x = 'Year')
}

update_hr_discards_txt <- function(
  in_file = "02-had/data/brottkast2001-2017.txt"
) {
  species_recode <- c(cod = 1, haddock = 2)
  gear_recode <- c()

  conv <- function(x) {
    x <- gsub(",", ".", x, fixed = TRUE)
    ifelse(x %in% c("NA", ""), NA_real_, as.numeric(x))
  }

  discards <-
    utils::read.table(in_file, header = TRUE, sep = "\t") |>
    dplyr::mutate(
      species = as.integer(forcats::fct_recode(
        species,
        "1" = "cod",
        "2" = "haddock"
      )),
      gear = forcats::fct_recode(
        gear,
        "Longlines" = 'line',
        "Bottom trawl" = 'trawl',
        "Danish seine" = 'seine',
        "Net" = "net"
      ),
      disc.n = conv(disc.n),
      disc.n.perc = conv(disc.n.perc),
      disc.wgt = conv(disc.wgt),
      disc.wgt.perc = conv(disc.wgt.perc),
      CV = conv(CV)
    )

  utils::write.table(
    discards,
    file = paste0("hafroreports/data/hr_discards.txt")
  )
}
