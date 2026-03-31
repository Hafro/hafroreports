hr_advice_ref_table <- function(ref_points, ref_points_basis_table) {
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
