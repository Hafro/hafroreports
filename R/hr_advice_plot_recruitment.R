#' Reshape wide assessment data into long format
#'
#' Pivots a wide assessment tibble (as returned by
#' \code{\link{hr_assessment_from_sag}} or assembled manually) into the long
#' format used by the advice plotting functions. Adds localised ordered factor
#' labels for each assessment variable in both English and Icelandic.
#'
#' @param assessment A wide-format tibble with columns \code{year},
#'   \code{species}, \code{assessment_year}, and columns named using the
#'   pattern \code{<stat>_<key>} (e.g. \code{median_SSB}, \code{low_HR}).
#' @param labels Labels replacing the default ones, e.g. for an index-based
#'   (category 3) stock \code{list(en = c(recruitment = "Juvenile index"),
#'   is = c(recruitment = "Nýliðunarvísitala"))}, named by key. Default
#'   \code{NULL}.
#' @return A long-format tibble with columns \code{year}, \code{species},
#'   \code{assessment_year}, \code{key}, \code{low}, \code{median},
#'   \code{high}, \code{label.is}, and \code{label.en}.
#' @export
hr_advice_data_assessment <- function(assessment, labels = NULL) {
  # NSE variables
  key <- value <- year <- species <- assessment_year <- stat <- NULL
  out <- assessment |>
    tidyr::gather(key, value, -c(year, species, assessment_year)) |>
    dplyr::filter(key != 'landings') |>
    tidyr::separate(key, c('stat', 'key')) |>
    tidyr::spread(stat, value) |>
    dplyr::mutate(
      label.is = ordered(
        forcats::fct_recode(
          key,
          'N\u00fdli\u00f0un' = 'recruitment',
          'Hrygningarstofn' = 'SSB',
          'Vi\u00f0mi\u00f0unarstofn' = 'refbio',
          'Vei\u00f0ihlutfall' = 'HR',
          # 'Landaður afli' = 'landings',
          'Vei\u00f0id\u00e1nartala' = 'F'
        ),
        levels = c(
          'N\u00fdli\u00f0un',
          'Hrygningarstofn',
          'Vi\u00f0mi\u00f0unarstofn',
          'Vei\u00f0ihlutfall',
          'Vei\u00f0id\u00e1nartala'
        )
      ),
      label.en = ordered(
        forcats::fct_recode(
          key,
          'Recruitment' = 'recruitment',
          'SSB' = 'SSB',
          'Reference biomass' = 'refbio',
          'Harvest rate' = 'HR',
          # 'Landaður afli' = 'landings',
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
  for (lang in names(labels)) {
    col <- paste0("label.", lang)
    old <- as.character(out[[col]])
    keys <- intersect(names(labels[[lang]]), out$key)
    for (k in keys) {
      old[out$key == k] <- labels[[lang]][[k]]
    }
    out[[col]] <- ordered(old, levels = unique(c(
      unname(unlist(labels[[lang]][keys])),
      levels(out[[col]])
    )))
  }
  out
}
#' Plot recruitment for advice sheet
#'
#' Bar chart of recruitment (millions) with confidence intervals for the
#' current assessment.
#'
#' @param data_assessment Long-format assessment data as returned by
#'   \code{\link{hr_advice_data_assessment}}.
#' @param assessment_year Integer. The assessment year to plot.
#' @param recruitment_age Recruitment age, shown in the title, or \code{NULL}
#'   for no age. Default \code{NULL}.
#' @param title Plot title, e.g. \code{hr_label("juvenile_index", bold = TRUE)} for
#'   a juvenile survey index. Default: recruitment (at age).
#' @param scale Divisor of the recruitment: \code{1e3} (default) for
#'   thousands shown in millions; e.g. \code{1} for an index shown as it is.
#' @param y_label Y axis label. Default millions, or none when \code{scale}
#'   is not \code{1e3}.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_recruitment <- function(
  data_assessment,
  assessment_year,
  recruitment_age = NULL,
  title = NULL,
  scale = 1e3,
  y_label = NULL
) {
  # NSE variables
  key <- low <- median <- high <- year <- NULL
  lang <- getOption("hr.lang", "en")
  millions <- isTRUE(scale == 1e3)
  if (is.null(y_label)) {
    y_label <- if (millions) hr_label("millions", bold = TRUE) else ''
  }

  data_assessment |>
    dplyr::filter(
      key == 'recruitment',
      assessment_year == .env$assessment_year
    ) |>
    dplyr::mutate(
      low = low / scale,
      median = median / scale,
      high = high / scale
    ) |>
    ggplot2::ggplot(ggplot2::aes(year, median)) +
    ggiraph::geom_bar_interactive(
      stat = 'identity',
      fill = 'deepskyblue',
      ggplot2::aes(
        tooltip = paste(
          eval(rlang::sym(paste('label', lang, sep = '.'))),
          ':',
          round(median),
          if (!millions) '' else if (lang == 'is') 'millj.' else 'mill.',
          '\n',
          hr_label("year"),
          ':',
          year
        ),
        data_id = year
      )
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = low, ymax = high),
      linewidth = 0.25
    ) +
    hr_astand_theme() +
    ggplot2::labs(
      y = y_label,
      title = if (!is.null(title)) {
        title
      } else if (is.null(recruitment_age)) {
        hr_label("recruitment", bold = TRUE)
      } else {
        hr_label("recruitment_age", recruitment_age, bold = TRUE)
      }
    ) +
    hr_advice_y_scale() +
    hr_astand_x_scale(5)
}
