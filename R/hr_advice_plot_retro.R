#' Plot retrospective assessment results for advice sheet
#'
#' Creates an interactive faceted line plot showing time series of HR, SSB,
#' reference biomass, and recruitment from up to five consecutive assessment
#' years. The most recent year is highlighted in red. Reference point lines
#' are overlaid on the relevant panels.
#'
#' @param data_assessment A long-format data frame as returned by
#'   \code{\link{hr_advice_data_assessment}}.
#' @param ref_points A named list or data frame with elements \code{HR_mgt},
#'   \code{HR_pa}, \code{HR_msy}, \code{B_pa}, \code{B_lim}, and
#'   \code{MGT_btrigger}.
#' @param assessment_year Integer. The most recent assessment year.
#' @return A \code{ggplot2} / \code{ggiraph} faceted plot object.
#' @export
hr_advice_plot_retro <- function(
  data_assessment,
  ref_points,
  assessment_year
) {
  # NSE variables
  key <- year <- median <- label <- label2 <- NULL
  lang <- getOption("hr.lang", "en")

  data_assessment |>
    dplyr::filter(
      key %in% c('HR', 'SSB', 'refbio', 'recruitment'),
      assessment_year > .env$assessment_year - 5,
      year >= .env$assessment_year - 15
    ) |>
    dplyr::filter(
      !(key == 'recruitment' & assessment_year < .env$assessment_year)
    ) |>
    #arrange(label) |>
    dplyr::mutate(
      label = eval(as.symbol(paste('label', lang, sep = '.'))), # gsub('(.+)~(.+)',"bold('\\1')~\\2",label),
      assessment_year = as.ordered(assessment_year),
      median = ifelse(key == 'HR', median, median / 1e3)
    ) |>
    ggplot2::ggplot(ggplot2::aes(
      x = year,
      y = median,
      color = assessment_year
    )) +
    ggplot2::scale_color_manual(
      values = c("black", "black", 'black', 'black', 'tomato3'),
      labels = scales::parse_format(),
      guide = ggplot2::guide_legend(label.position = 'right')
    ) +
    ggiraph::geom_line_interactive(
      size = 0.5,
      ggplot2::aes(
        tooltip = paste(
          if (lang == 'is') 'Ráðgjafarár' else 'Assessment year',
          ':',
          assessment_year
        ),
        data_id = assessment_year
      )
    ) +
    #geom_line(size=0.5) +
    ggplot2::facet_wrap(
      ~label,
      labeller = ggplot2::label_value,
      scales = 'free'
    ) +
    ggplot2::labs(y = '') +
    ggplot2::geom_hline(
      data = tibble::tibble(
        year = rep(assessment_year - 10, 5),
        median = c(
          ref_points$HR_mgt,
          ref_points$HR_msy, # ref_points$HR_lim,
          ref_points$HR_pa,
          ref_points$B_pa,
          ref_points$B_lim
        ),
        label = c(
          rep(
            if (lang == 'is') "Veiðihlutfall" else "Harvest rate",
            3
          ),
          rep(if (lang == 'is') "Hrygningarstofn" else "SSB", 2)
        ),
        #c(rep("bold('Veiðihlutfall')~italic('Harvest rate')",4),
        #           rep("bold('Hrygningarstofn')~italic('SSB')",2)),
        label2 = sapply(c("HRmgt", "HRmsy", "HRpa", "Bpa", "Blim"), hr_label)
      ),
      ggplot2::aes(yintercept = median),
      linetype = "dashed",
      size = 0.4
    ) +
    ggplot2::geom_text(
      data = tibble::tibble(
        year = c(
          assessment_year - 14,
          assessment_year - 11,
          assessment_year - 0.25,
          assessment_year - 12,
          assessment_year - 13,
          assessment_year - 5
        ),
        median = c(
          1.1 * ref_points$HR_mgt,
          1.1 * ref_points$HR_msy,
          1.1 * ref_points$HR_pa, #1.05*ref_points$HR_lim,
          1.15 * ref_points$B_pa,
          1.15 * ref_points$B_lim,
          1.15 * ref_points$MGT_btrigger
        ),
        label = c(
          rep(
            if (lang == 'is') "Veiðihlutfall" else "Harvest rate",
            3
          ),
          rep(if (lang == 'is') "Hrygningarstofn" else "SSB", 3)
        ),
        label2 = sapply(
          c("HRmgt", "HRmsy", "HRpa", "Bpa", "Blim", "Btrigger"),
          hr_label
        )
      ),
      ggplot2::aes(label = label2),
      parse = TRUE,
      size = 2.5,
      color = 'black'
    ) +
    hr_astand_theme(legend.position = 'none') +
    ggplot2::theme(
      legend.position = 'none',
      strip.text = ggplot2::element_text(face = "bold")
    ) +
    hr_astand_x_scale(4, limits = c(assessment_year - 15, assessment_year)) +
    ggplot2::expand_limits(y = 0)
}
