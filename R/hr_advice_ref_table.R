#' Format reference points table for advice sheet
#'
#' Joins reference point values with their basis descriptions and renders a
#' formatted \code{flextable} with columns for approach, reference point name,
#' value, and basis. Column headers and cell content are localised to the
#' current language setting. Markdown in the basis column is rendered via
#' \code{ftExtra::colformat_md}.
#'
#' @param ref_points A named list or one-row data frame of reference point
#'   values (e.g. \code{HR_mgt}, \code{B_lim}). \code{NA} values are dropped.
#' @param ref_points_basis_table A data frame with columns \code{ref_point},
#'   \code{render}, \code{approach.en}, \code{approach.is}, \code{basis.en},
#'   and \code{basis.is} describing each reference point.
#' @param biomass_multiplier Multiplier for the biomass reference points
#'   (\code{B_*}, \code{*btrigger}), to show values given in thousand tonnes
#'   in tonnes. Default \code{1000}; use \code{1} if they are already in
#'   tonnes.
#' @return A \code{flextable} object styled for inclusion in an advice sheet.
#' @export
hr_advice_ref_table <- function(
  ref_points,
  ref_points_basis_table,
  biomass_multiplier = 1000
) {
  # Biomass reference points are kept in thousand tonnes for the figures;
  # show them in tonnes
  ref_points <- hr_ref_points_tonnes(ref_points, biomass_multiplier)
  # NSE variables
  ref_point <- value <- approach <- basis <- render <- NULL
  lang <- getOption("hr.lang", "en")

  ref_table <-
    ref_points |>
    as.data.frame() |>
    tidyr::pivot_longer(dplyr::everything(), names_to = 'ref_point') |>
    stats::na.omit() |>
    dplyr::right_join(ref_points_basis_table) |>
    dplyr::select(
      approach = rlang::sym(paste0('approach.', lang)),
      render,
      value,
      basis = rlang::sym(paste0('basis.', lang))
    ) |>
    dplyr::arrange(approach) |>
    dplyr::mutate(
      approach = dplyr::case_when(
        is.na(dplyr::lag(approach)) ~ approach,
        approach == dplyr::lag(approach) ~ '',
        TRUE ~ approach
      ),
      value = ifelse(value < 1, value, hr_red_dot_number(round(value)))
    )

  flextable::flextable(ref_table) |>
    flextable::mk_par(
      j = "approach",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Nálgun" else 'Approach'
      )
    ) |>
    flextable::mk_par(
      j = "render",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Viðmiðunarmörk" else 'Reference point'
      )
    ) |>
    flextable::mk_par(
      j = "value",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Gildi" else 'Value'
      )
    ) |>
    flextable::mk_par(
      j = "basis",
      part = "header",
      value = flextable::as_paragraph(
        if (lang == 'is') "Grundvöllur" else 'Basis'
      )
    ) |>
    ftExtra::colformat_md() |>
    flextable::valign(j = 1:4, valign = "top", part = "body") |>
    flextable::bg(j = 1:4, bg = "#DEEAF6", part = "header") |>
    ### Total width of table in advice sheet is 9
    flextable::width(j = 1, width = 1.25) |>
    flextable::width(j = 2, width = 1.5) |>
    flextable::width(j = 3, width = 1.) |>
    flextable::width(j = 4, width = 5.25) |>
    flextable::line_spacing(space = 1.1, part = "all") |>
    flextable::padding(padding = 2, part = "body") |>
    flextable::align(j = 3, align = "center", part = "all") |>
    flextable::border_remove() |>
    flextable::border_outer(part = "all", border = officer::fp_border()) |>
    flextable::border(
      j = c(2, 3, 4),
      border.left = officer::fp_border(),
      part = "all"
    ) |>
    flextable::border(
      j = c(2, 3, 4),
      border.bottom = officer::fp_border(),
      part = "body"
    ) |>
    flextable::border(i = 2, j = 1, border.bottom = officer::fp_border()) |>
    flextable::border(i = 4, j = 1, border.bottom = officer::fp_border()) |>
    flextable::fontsize(size = 9, part = "body") |>
    flextable::fontsize(size = 9, part = "header")
}

#' Biomass reference points in tonnes
#'
#' Reference points are usually kept with biomass in thousand tonnes, as the
#' advice figures plot biomass in thousand tonnes. This converts the biomass
#' reference points (names starting \code{B_} or ending \code{btrigger}) for
#' tables or plots in tonnes, leaving fishing mortality and harvest rates as
#' they are.
#'
#' @param ref_points Named list of reference points.
#' @param multiplier Multiplier for the biomass reference points. Default
#'   \code{1000}.
#' @return \code{ref_points} with the biomass reference points multiplied.
#' @export
hr_ref_points_tonnes <- function(ref_points, multiplier = 1000) {
  biomass <- grepl("^B_|btrigger$", names(ref_points))
  ref_points[biomass] <- lapply(ref_points[biomass], function(x) multiplier * x)
  ref_points
}
