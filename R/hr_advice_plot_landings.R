# Display labels and colours for landings gear groups, in stacking order
# (first at the bottom of the bars)
advice_gear_groups <- tibble::tribble(
  ~gear_name, ~gear.en, ~gear.is, ~colour,
  "LLN", "Longline", "L\u00edna", "tomato3",
  "DSE", "Demersal seine", "Dragn\u00f3t", "navajowhite3",
  "GIL", "Gillnet", "Net", "tomato3",
  "HLN", "Handline", "Handf\u00e6ri", "darkseagreen4",
  "NPT", "Nephrops trawl", "Humarvarpa", "khaki3",
  "BMT", "Bottom trawl", "Botnvarpa", "steelblue3",
  "Other", "Other and undefined gear", "Anna\u00f0 og \u00f3skilgreint", "black"
)

#' Prepare landings data for advice plots and tables
#'
#' Summarises landings from a gear-grouped data frame into thousands of tonnes
#' per year per gear, and adds localised English and Icelandic gear name
#' factors in the display order used by advice sheet figures.
#'
#' @param landings_by_gear A data frame or lazy table with columns \code{year},
#'   \code{gear_name} (e.g. \code{"BMT"}, \code{"NPT"} (Nephrops trawl),
#'   \code{"DSE"}, \code{"LLN"}, \code{"GIL"}, \code{"HLN"},
#'   \code{"Other"}, as produced by
#'   \code{pax::pax_landings_by_gear()} with the stock's gear groups), and
#'   \code{catch} (kg). Unknown gear names are shown under their own name.
#' @return A tibble with columns \code{year}, \code{gear_name},
#'   \code{tonnes} (landings in thousands of tonnes), \code{gear.is}
#'   (ordered Icelandic gear factor), and \code{gear.en} (ordered English
#'   gear factor).
#' @export
hr_advice_data_landings <- function(landings_by_gear) {
  # NSE variables
  year <- gear_name <- catch <- NULL

  out <- landings_by_gear |>
    dplyr::collect() |>
    dplyr::mutate(gear_name = dplyr::coalesce(gear_name, "Other")) |>
    dplyr::group_by(year, gear_name) |>
    dplyr::summarise(tonnes = sum(catch) / 1e6, .groups = "drop")
  gears <- advice_gear_groups[advice_gear_groups$gear_name %in% out$gear_name, ]
  extra <- setdiff(unique(out$gear_name), gears$gear_name)
  levels_en <- c(gears$gear.en, extra)
  levels_is <- c(gears$gear.is, extra)
  out |>
    dplyr::mutate(
      gear.en = factor(
        ifelse(gear_name %in% gears$gear_name, gears$gear.en[match(gear_name, gears$gear_name)], gear_name),
        levels = levels_en
      ),
      gear.is = factor(
        ifelse(gear_name %in% gears$gear_name, gears$gear.is[match(gear_name, gears$gear_name)], gear_name),
        levels = levels_is
      )
    )
}

#' Plot landings by gear for advice sheet
#'
#' Creates an interactive stacked bar chart showing total landings by gear
#' type over time, with colours and labels adjusted for the current language
#' setting. Each gear group keeps its colour whichever groups the stock uses.
#'
#' @param data_landings A data frame as returned by
#'   \code{\link{hr_advice_data_landings}}.
#' @param assessment_year Integer. Used to set the x-axis upper limit.
#' @param year_start First year on the x axis. Default 1978.
#' @param legend_position Legend position inside the panel. Default
#'   \code{c(0.35, 0.85)}.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_landings <- function(
  data_landings,
  assessment_year,
  legend_position = c(0.35, 0.85),
  year_start = 1978
) {
  # NSE variables
  year <- fill <- tonnes <- ymax <- ymin <- .data <- NULL
  lang <- getOption("hr.lang", "en")
  label_col <- paste0("gear.", lang)

  stacked <- data_landings |>
    # Years outside the axis would still set the y scale
    dplyr::filter(year >= year_start) |>
    dplyr::mutate(fill = .data[[label_col]]) |>
    dplyr::arrange(year, fill) |>
    dplyr::group_by(year) |>
    dplyr::mutate(
      ymax = cumsum(tonnes),
      ymin = ymax - tonnes
    )
  fill_levels <- levels(data_landings[[label_col]])
  colours <- advice_gear_groups$colour[match(fill_levels, advice_gear_groups[[label_col]])]
  # Unknown gear groups, and gear groups sharing a colour (gillnet and
  # longline), get the next unused fallback colour
  fallback <- c("goldenrod3", "darkseagreen4", "orchid4", "grey60", "grey30")
  for (i in seq_along(colours)) {
    if (is.na(colours[i]) || colours[i] %in% colours[seq_len(i - 1)]) {
      colours[i] <- setdiff(fallback, colours)[1]
    }
  }
  names(colours) <- fill_levels

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
          if (lang == 'is') '\u00c1r' else 'Year',
          ': ',
          year
        ),
        data_id = interaction(year, fill)
      )
    ) +
    ggplot2::scale_fill_manual(
      values = colours,
      guide = ggplot2::guide_legend(reverse = TRUE, label.position = "right")
    ) +
    ggplot2::labs(
      y = hr_label("thousand_tonnes", bold = TRUE),
      title = hr_label("catches", bold = TRUE)
    ) +
    hr_astand_theme(legend.position = legend_position) +
    hr_astand_x_scale(5, 0, limits = c(year_start, assessment_year - 0.5)) +
    hr_advice_y_scale() +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold"), # Facet titles
      axis.title.y = ggplot2::element_text(face = "bold"), # Y-axis title
      plot.title = ggplot2::element_text(face = "bold")
    )
}

#' Y axis for advice sheet figures
#'
#' Starts at 0, with the top and breaks following the data, so figures fit
#' any stock.
#'
#' @param ... Passed to \code{ggplot2::scale_y_continuous()}.
#' @return A ggplot2 scale.
#' @export
hr_advice_y_scale <- function(...) {
  ggplot2::scale_y_continuous(
    breaks = scales::breaks_pretty(n = 6),
    limits = c(0, NA),
    expand = ggplot2::expansion(mult = c(0, 0.05)),
    ...
  )
}
