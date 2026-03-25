hr_advice_data_assessment <- function(assessment) {
  assessment |>
    tidyr::gather(key, value, -c(year, species, assessment_year)) |>
    dplyr::filter(key != 'landings') |>
    tidyr::separate(key, c('stat', 'key')) |>
    tidyr::spread(stat, value) |>
    dplyr::mutate(
      label.is = ordered(
        forcats::fct_recode(
          key,
          'Nýliðun' = 'recruitment',
          'Hrygningarstofn' = 'SSB',
          'Viðmiðunarstofn' = 'refbio',
          'Veiðihlutfall' = 'HR',
          #'Landaður afli' = 'landings',
          'Veiðidánartala' = 'F'
        ),
        levels = c(
          'Nýliðun',
          'Hrygningarstofn',
          'Viðmiðunarstofn',
          'Veiðihlutfall',
          'Veiðidánartala'
        )
      ),
      label.en = ordered(
        forcats::fct_recode(
          key,
          'Recruitment' = 'recruitment',
          'SSB' = 'SSB',
          'Reference biomass' = 'refbio',
          'Harvest rate' = 'HR',
          #'Landaður afli' = 'landings',
          'Fishing mortality' = 'F'
        ),
        levels = c(
          'Recruitment',
          'SSB',
          'Reference biomass',
          'Harvest rate',
          'Fishing mortality'
        )
      )
    )
}
hr_advice_plot_recruitment <- function(
  data_assessment,
  assessment_year
) {
  lang <- getOption("hr.lang", "en")

  data_assessment |>
    dplyr::filter(key == 'recruitment', assessment_year == assessment_year) |>
    ggplot2::ggplot(ggplot2::aes(year, median / 1000)) +
    ggiraph::geom_bar_interactive(
      stat = 'identity',
      fill = 'deepskyblue',
      ggplot2::aes(
        tooltip = paste(
          eval(rlang::sym(paste('label', lang, sep = '.'))),
          ':',
          round(median / 1e3),
          'mill.',
          '\n',
          hr_label("year"),
          ':',
          year
        ),
        data_id = year
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = low / 1000, ymax = high / 1000),
      size = 0.25
    ) +
    hr_astand_theme() +
    ggplot2::labs(
      y = hr_label("million_tonnes", bold = TRUE),
      title = hr_label("recruitment_age", 1, bold = TRUE)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 600, 100),
      expand = c(0, 0),
      limits = c(0, 600)
    ) +
    hr_astand_x_scale(5)
}
