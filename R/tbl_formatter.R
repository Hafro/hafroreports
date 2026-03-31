#' Format a data frame as a styled GT table
#'
#' Converts a data frame to a \code{gt} table with column header shading,
#' vertical column borders, missing value replacement, and thousands-separator
#' formatting for large numeric columns. Column names containing \code{"__"}
#' are split on that separator: the text before becomes a spanner header and
#' the text after becomes the column label.
#'
#' @param x A data frame to render as a GT table.
#' @param banner_col Character. Background colour for column headers and
#'   spanners. Default is \code{"#D3E7E0"}.
#' @return A \code{gt_tbl} object.
tbl_formater <- function(x, banner_col = "#D3E7E0") {
  x |>
    gt::gt() |>
    gt::sub_missing(
      columns = dplyr::everything(),
      rows = dplyr::everything(),
      missing_text = "---"
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = banner_col),
      locations = gt::cells_column_labels(dplyr::everything())
    ) |>

    # Engar láréttar línur í haus eða undir töflunni
    gt::tab_options(
      table.font.names = c("Georgia", "Times New Roman", "serif")
      #   table.border.top.style = "none",
      #   table.border.bottom.style = "none",
      #   heading.border.bottom.style = "none",
      #   column_labels.border.top.style = "none",
      #   column_labels.border.bottom.style = "none"
    ) |>

    # Fjarlægja láréttar línur milli raða
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("top", "bottom"),
        color = "white",
        weight = gt::px(0)
      ),
      locations = gt::cells_body(rows = dplyr::everything())
    ) |>

    # Bæta við lóðréttum línum milli dálka
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right", "left"),
        color = "black",
        weight = gt::px(1)
      ),
      locations = gt::cells_body(columns = dplyr::everything())
    ) |>

    # Sama fyrir hausinn
    gt::tab_style(
      style = gt::cell_borders(
        sides = c("right", 'left'),
        color = "black",
        weight = gt::px(1)
      ),
      locations = gt::cells_column_labels(columns = dplyr::everything())
    ) -> gt_tbl

  numeric_cols <- names(x)[sapply(x, is.numeric)]
  # Format-a hvern dálk sem hefur gildi ≥ 1000
  for (col in numeric_cols[-1]) {
    gt_tbl <- gt_tbl |>
      gt::fmt_number(
        columns = dplyr::all_of(col),
        rows = x[[col]] >= 1000,
        sep_mark = " ",
        decimals = 0
      )
  }

  if (sum(grepl('__', names(x)))) {
    cols <- names(x)
    spanners <- gsub("__.*", "", cols)
    labels <- gsub(".*__", "", cols)

    # Búa til list fyrir cols_label
    label_list <- setNames(labels, cols)

    # Búa til gt töflu
    gt_tbl <-
      gt_tbl |>
      gt::cols_label(!!!label_list)

    # Bæta spanners (nema Year)
    for (spanner in unique(spanners[-1])) {
      target_cols <- cols[spanners == spanner]
      gt_tbl <- gt_tbl |>
        gt::tab_spanner(label = spanner, columns = dplyr::all_of(target_cols))
    }
    gt_tbl <-
      gt_tbl |>
      gt::tab_style(
        style = gt::cell_fill(color = banner_col),
        locations = gt::cells_column_spanners(dplyr::everything())
      )
  }

  gt_tbl
}
