#' Plot retrospective comparison of recent assessments for advice sheet
#'
#' Faceted line plot comparing the current assessment (red) with the
#' assessments of the previous years (black) over the last 15 years: fishing
#' pressure (harvest rate or F), SSB, the reference biomass (if the stock has
#' one) and recruitment. Dashed lines show the reference points the stock
#' has.
#'
#' @param data_assessment Long-format assessment data as returned by
#'   \code{\link{hr_advice_data_assessment}}, with several assessment years.
#' @param ref_points Named list of reference points (biomass in thousand
#'   tonnes).
#' @param assessment_year Integer. The current assessment year.
#' @param fishing_pressure \code{"HR"} (harvest rate) or \code{"F"}. Default
#'   \code{"HR"}.
#' @param recruitment_from First assessment year whose recruitment is shown,
#'   e.g. the year the recruitment age changed (earlier assessments estimated
#'   recruitment at another age). Default \code{NULL}, all assessments.
#' @param biomass_scale Divisor of the biomass and recruitment: \code{1000}
#'   (default) for thousand tonnes (millions), \code{1} for relative biomass
#'   (B/B_MSY) shown as it is.
#' @param show_lim If \code{TRUE}, also draw the limit reference point of
#'   the fishing pressure (\code{F_lim} or \code{HR_lim}). Default
#'   \code{FALSE}.
#' @param panel_order Keys in the order of the panels, e.g.
#'   \code{c("F", "recruitment", "SSB")}, the same in every language.
#'   Default \code{NULL}: alphabetical by label, as before.
#' @return A \code{ggplot2} / \code{ggiraph} plot object.
#' @export
hr_advice_plot_retro <- function(
  data_assessment,
  ref_points,
  assessment_year,
  fishing_pressure = c("HR", "F"),
  recruitment_from = NULL,
  biomass_scale = 1000,
  show_lim = FALSE,
  panel_order = NULL
) {
  # NSE variables
  key <- year <- median <- label <- value <- facet <- label2 <- .data <- NULL
  lang <- getOption("hr.lang", "en")
  fishing_pressure <- match.arg(fishing_pressure)
  rec_from <- if (is.null(recruitment_from)) -Inf else recruitment_from

  d <- data_assessment |>
    dplyr::filter(
      key %in% c(fishing_pressure, 'SSB', 'refbio', 'recruitment'),
      assessment_year > .env$assessment_year - 5,
      year >= .env$assessment_year - 15
    ) |>
    dplyr::filter(
      key != 'recruitment' | assessment_year >= rec_from
    ) |>
    dplyr::group_by(key) |>
    dplyr::filter(any(!is.na(median))) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      label = as.character(.data[[paste('label', lang, sep = '.')]]),
      assessment_year = as.ordered(assessment_year),
      median = ifelse(key == fishing_pressure, median, median / biomass_scale)
    )
  label_of <- function(k) unique(d$label[d$key == k])
  if (!is.null(panel_order)) {
    keys <- c(intersect(panel_order, unique(d$key)), setdiff(unique(d$key), panel_order))
    panel_levels <- unique(unlist(lapply(keys, label_of)))
  }
  n_years <- nlevels(droplevels(d$assessment_year))

  fp_refs <- advice_ref_lines(ref_points, fishing_pressure, show_lim = show_lim)
  b_refs <- tibble::tibble(
    value = c(ref_points$B_pa, ref_points$B_lim),
    label = c(hr_label("Bpa"), hr_label("Blim"))
  )
  refs <- dplyr::bind_rows(
    if (length(label_of(fishing_pressure))) dplyr::mutate(fp_refs, facet = label_of(fishing_pressure)),
    if (length(label_of('SSB'))) dplyr::mutate(b_refs, facet = label_of('SSB'))
  )
  if (!is.null(refs) && nrow(refs)) {
    refs <- refs |>
      dplyr::filter(!is.na(value)) |>
      dplyr::rename(label2 = label, label = facet) |>
      dplyr::mutate(year = assessment_year - 14 + 3 * (dplyr::row_number() - 1) %% 4)
  }

  if (!is.null(panel_order)) {
    d$label <- factor(d$label, levels = panel_levels)
    if (!is.null(refs) && nrow(refs)) {
      refs$label <- factor(refs$label, levels = panel_levels)
    }
  }

  ggplot2::ggplot(d, ggplot2::aes(
    x = year,
    y = median,
    color = assessment_year
  )) +
    ggplot2::scale_color_manual(
      values = c(rep("black", max(n_years - 1, 0)), 'tomato3'),
      guide = ggplot2::guide_legend(label.position = 'right')
    ) +
    ggiraph::geom_line_interactive(
      linewidth = 0.5,
      ggplot2::aes(
        tooltip = paste(
          if (lang == 'is') 'Ráðgjafarár' else 'Assessment year',
          ':',
          assessment_year
        ),
        data_id = assessment_year
      )
    ) +
    ggplot2::facet_wrap(
      ~label,
      labeller = ggplot2::label_value,
      scales = 'free'
    ) +
    ggplot2::labs(y = '') +
    ggplot2::geom_hline(
      data = refs,
      ggplot2::aes(yintercept = value),
      linetype = "dashed",
      linewidth = 0.4
    ) +
    ggplot2::geom_text(
      data = refs,
      ggplot2::aes(x = year, y = 1.1 * value, label = label2),
      inherit.aes = FALSE,
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
