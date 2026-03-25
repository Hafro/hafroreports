hr_advice_data_landings <- function(landings_by_gear) {
  # Was advice/tables/landings.csv
  landings_by_gear |>
    dplyr::group_by(year, gear_name) |>
    dplyr::summarise(tonnes = sum(catch) / 1e6, .groups = "drop") |>
    # TODO: pax::pax_describe_mfdb_gear_code? We'd need icelandic
    dplyr::collect() |>
    dplyr::mutate(
      gear.is = forcats::fct_recode(
        gear_name,
        "Lína" = "LLN",
        "Dragnót" = "DSE",
        "Botnvarpa" = "BMT",
        "Annað og óskilgreint" = "Other"
      ) |>
        forcats::fct_relevel(
          "Lína",
          "Dragnót",
          "Botnvarpa",
          "Annað og óskilgreint"
        ),

      gear.en = forcats::fct_recode(
        gear_name,
        "Longline" = "LLN",
        "Demersal seine" = "DSE",
        "Bottom trawl" = "BMT",
        "Other and undefined gear" = "Other"
      ) |>
        forcats::fct_relevel(
          "Longline",
          "Demersal seine",
          "Bottom trawl",
          "Other and undefined gear"
        )
    )
}

hr_advice_plot_landings <- function(
  landings_data,
  assessment_year
) {
  lang <- getOption("hr.lang", "en")

  stacked <- landings_data |>
    dplyr::mutate(fill = !!as.symbol(paste0("gear.", lang))) |>
    dplyr::arrange(year, fill) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      ymin = cumsum(lag(tonnes, default = 0)),
      ymax = ymin + tonnes
    )

  ggplot2::ggplot(stacked, ggplot2::aes(x = year, fill = fill)) +
    ggiraph::geom_rect_interactive(
      ggplot2::aes(
        xmin = year - 0.4,
        xmax = year + 0.4,
        ymin = ymin,
        ymax = ymax,
        tooltip = paste0(
          fill,
          ": ",
          round(tonnes * 1e3),
          " t",
          '\n',
          if (lang == 'is') 'Ár' else 'Year',
          ': ',
          year
        ),
        data_id = interaction(year, fill)
      )
    ) +
    ggplot2::scale_fill_manual(
      # TODO: The original 4 is expecting only 3 gears + other
      values = c(
        "tomato3",
        "navajowhite3",
        "steelblue3",
        "black",
        rep("black", 9)
      ),
      guide = ggplot2::guide_legend(reverse = TRUE, label.position = "right")
    ) +
    ggplot2::labs(
      y = hr_label("thousand_tonnes", bold = TRUE),
      title = hr_label("catch", 1, bold = TRUE)
    ) +
    hr_astand_theme(legend.position = c(0.35, 0.85)) +
    hr_astand_x_scale(5, 0, limits = c(1978, assessment_year - 0.5)) +
    ggplot2::scale_y_continuous(
      breaks = seq(0, 160, 20),
      expand = c(0, 0),
      limits = c(0, 120)
    ) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"), # Facet titles
      axis.title.y = ggplot2::element_text(face = "bold"), # Y-axis title
      plot.title = ggplot2::element_text(face = "bold")
    )
}
