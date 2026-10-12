#' Tech report figures of invertebrate (shrimp) survey and fishery stocks
#'
#' Figures of the inshore and offshore shrimp tech reports, from the pax
#' shrimp survey functions (\code{pax::pax_si_catch_weight_biomass()},
#' \code{pax::pax_si_stratified_index()}) and summaries of the landings and
#' logbooks. Labels follow \code{options(hr.lang = )}.
#'
#' @name hr_techreport_plot_invertebrate
NULL

#' @param survey_index Long data frame of indices with columns \code{index},
#'   \code{year}, \code{b}, \code{b_lower} and \code{b_upper}.
#' @param indices Named character vector: the indices to show (values of
#'   \code{survey_index$index}) and their \code{hr_label()} keys (names are
#'   the indices). Default total, female, fishable and juvenile biomass.
#' @param ref_lines Named list of horizontal lines by index, e.g.
#'   \code{list(fishable = 390)} (I_lim).
#' @param survey_index2 Optional second survey in the same format (e.g. the
#'   February survey of Isafjardardjup), drawn in grey with a dashed line
#'   over the first. \code{NULL} (default) for none.
#' @return \subsection{hr_techreport_plot_survey_indices}{A ggplot: one panel
#'   per index (own y scale), with the 95 percent interval as a band}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_survey_indices <- function(
  survey_index,
  indices = c(
    total = "total_biomass_index",
    female = "female_biomass_index",
    fishable = "fishable_biomass_index",
    juvenile = "juvenile_biomass_index"
  ),
  ref_lines = NULL,
  survey_index2 = NULL
) {
  # NSE variables
  index <- year <- b <- b_lower <- b_upper <- label <- value <- NULL

  labels <- vapply(indices, hr_label, character(1))
  prep <- function(x) {
    x |>
      dplyr::filter(index %in% names(indices)) |>
      dplyr::mutate(label = factor(labels[index], levels = labels))
  }
  d <- prep(survey_index)
  p <- ggplot2::ggplot(d, ggplot2::aes(year, b)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = b_lower, ymax = b_upper), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 0.8)
  if (!is.null(survey_index2)) {
    # Gaps between the years of an irregular survey (no line across them)
    d2 <- prep(survey_index2) |>
      tidyr::complete(
        year = seq(min(year), max(year)),
        tidyr::nesting(index, label)
      )
    p <- p +
      ggplot2::geom_ribbon(
        data = d2,
        ggplot2::aes(ymin = b_lower, ymax = b_upper),
        alpha = 0.2,
        colour = "grey"
      ) +
      ggplot2::geom_line(data = d2, linetype = 2) +
      ggplot2::geom_point(data = d2, colour = "grey50", size = 0.8)
  }
  p <- p +
    ggplot2::facet_wrap(~label, scales = "free_y", ncol = 2) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"))
  if (length(ref_lines) > 0) {
    refs <- data.frame(
      index = names(ref_lines),
      value = unlist(ref_lines)
    )
    refs$label <- factor(labels[refs$index], levels = labels)
    p <- p +
      ggplot2::geom_hline(
        data = refs,
        ggplot2::aes(yintercept = value),
        linetype = 2
      )
  }
  p
}

#' @param index_subarea Data frame with \code{year}, \code{stratum}
#'   (sub-area), \code{index} and \code{b} (the sub-area's part of the
#'   index: its mean station biomass times its area).
#' @param index The index to show, e.g. \code{"fishable"}.
#' @param title Plot title.
#' @return \subsection{hr_techreport_plot_index_subarea}{A ggplot: stacked
#'   bars of the index by sub-area and year}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_index_subarea <- function(
  index_subarea,
  index = "fishable",
  title = hr_label("fishable_biomass_index")
) {
  # NSE variables
  year <- b <- stratum <- NULL
  idx <- index
  d <- index_subarea[index_subarea$index == idx, ]
  ggplot2::ggplot(d, ggplot2::aes(year, b, fill = factor(stratum))) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::scale_fill_viridis_d(name = hr_label("subarea"), option = "D", end = 0.9) +
    ggplot2::labs(x = NULL, y = hr_label("tonnes"), title = title) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::theme_bw()
}

#' @param ldist Data frame with \code{year}, \code{length}, \code{sex} (1
#'   male, 2 female) and \code{b} (biomass index at length).
#' @param ncol Number of columns of panels.
#' @param ldist2 Optional second survey in the same format (e.g. the
#'   February survey, with \code{year} the year of the autumn survey before
#'   it), drawn with dashed lines in the panels of \code{ldist}.
#'   \code{NULL} (default) for none.
#' @return \subsection{hr_techreport_plot_ldist_sex}{A ggplot: the biomass
#'   index by length for each year, males black and females red, over the
#'   mean of all years (grey)}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_ldist_sex <- function(ldist, ncol = 5, ldist2 = NULL) {
  # NSE variables
  year <- length <- sex <- b <- NULL
  mean_all <- ldist |>
    dplyr::group_by(length, sex) |>
    dplyr::summarise(b = mean(b), .groups = "drop")
  p <- ggplot2::ggplot() +
    ggplot2::geom_area(
      data = mean_all[mean_all$sex == 1, ],
      ggplot2::aes(length, b),
      colour = "grey80",
      fill = "grey80"
    ) +
    ggplot2::geom_area(
      data = mean_all[mean_all$sex == 2, ],
      ggplot2::aes(length, b),
      colour = "grey80",
      fill = "grey80"
    ) +
    ggplot2::geom_line(data = ldist[ldist$sex == 1, ], ggplot2::aes(length, b)) +
    ggplot2::geom_line(data = ldist[ldist$sex == 2, ], ggplot2::aes(length, b), col = 2)
  if (!is.null(ldist2)) {
    ldist2 <- ldist2[ldist2$year %in% ldist$year, ]
    p <- p +
      ggplot2::geom_line(data = ldist2[ldist2$sex == 2, ], ggplot2::aes(length, b), col = 2, linetype = 2) +
      ggplot2::geom_line(data = ldist2[ldist2$sex == 1, ], ggplot2::aes(length, b), linetype = 2)
  }
  p +
    ggplot2::facet_wrap(~year, ncol = ncol, scales = "free_y") +
    ggplot2::labs(x = hr_label("carapace_length_mm"), y = hr_label("biomass_index")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8),
      strip.background = ggplot2::element_rect(fill = "white")
    )
}

#' @param station_catch Data frame with \code{year}, \code{lat}, \code{lon}
#'   and \code{catch_nm} (catch per nautical mile, kg; 0 where none was
#'   caught).
#' @param xlim,ylim Map limits (longitude, latitude).
#' @param size_label Title of the size legend, default kilograms per nm
#'   (e.g. \code{hr_label("number_per_nm")} for numbers of fish).
#' @return \subsection{hr_techreport_plot_station_catch}{A map for each
#'   year: the catch at the stations as circles, stations without catch as
#'   x. If \code{station_catch} has a column \code{season} (e.g. autumn and
#'   February surveys), one row of maps per season}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_station_catch <- function(
  station_catch,
  xlim,
  ylim,
  ncol = 5,
  size_label = hr_label("kg_per_nm")
) {
  # NSE variables
  lon <- lat <- catch_nm <- NULL
  pos <- station_catch[station_catch$catch_nm > 0, ]
  zero <- station_catch[station_catch$catch_nm == 0, ]
  pax::pax_map_base(xlim = xlim, ylim = ylim) +
    ggplot2::geom_point(
      data = pos,
      ggplot2::aes(lon, lat, size = catch_nm),
      alpha = 0.5,
      colour = "red"
    ) +
    ggplot2::geom_point(data = zero, ggplot2::aes(lon, lat), shape = 4, size = 1) +
    ggplot2::scale_size_area(max_size = 7, name = size_label) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    (if ("season" %in% names(station_catch)) {
      ggplot2::facet_grid(season ~ year)
    } else {
      ggplot2::facet_wrap(~year, ncol = ncol)
    }) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      legend.title = ggplot2::element_text(face = "italic", size = 8),
      # pax_map_base() hides the facet labels
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text.x = ggplot2::element_text(size = 7),
      panel.spacing = ggplot2::unit(0.1, "cm"),
      axis.text = ggplot2::element_text(size = 6)
    )
}

#' @param logbook_catch Data frame with \code{fishing_year}, \code{x},
#'   \code{y} (cell centres, longitude and latitude) and \code{catch} (kg),
#'   aggregated on a grid.
#' @param dx,dy Cell width and height (degrees), the grid of the data.
#' @return \subsection{hr_techreport_plot_logbook_catch}{A map for each
#'   fishing year of the logbook catch (tonnes) on the grid}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_logbook_catch <- function(
  logbook_catch,
  xlim,
  ylim,
  ncol = 5,
  dx = 0.07,
  dy = 0.06
) {
  # NSE variables
  x <- y <- catch <- long <- lat <- NULL
  # Cells of fixed size (a panel with one row of cells would otherwise get
  # cells 1 degree high), the land drawn over them as in the old figures
  pax::pax_map_base(xlim = xlim, ylim = ylim) +
    ggplot2::geom_tile(
      data = logbook_catch,
      ggplot2::aes(x, y, fill = catch / 1e3),
      width = dx,
      height = dy
    ) +
    ggplot2::geom_polygon(
      data = ggplot2::map_data("mapdata::worldHires", "Iceland"),
      ggplot2::aes(long, lat),
      fill = "gray70",
      colour = "black",
      linewidth = 0.2
    ) +
    ggplot2::scale_fill_viridis_c(option = "C", direction = -1, name = hr_label("catch_t_map")) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_pretty(n = 3)) +
    ggplot2::facet_wrap(~fishing_year, ncol = ncol) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme(
      # pax_map_base() hides the facet labels
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text.x = ggplot2::element_text(size = 7),
      panel.spacing = ggplot2::unit(0.1, "cm"),
      axis.text = ggplot2::element_text(size = 6)
    )
}

#' @param cpue Data frame with \code{year} (first year of the fishing year),
#'   \code{catch} (tonnes) and \code{cpue} (kg/h).
#' @return \subsection{hr_techreport_plot_catch_cpue}{Two panels: catch and
#'   CPUE by fishing year}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_catch_cpue <- function(cpue) {
  # NSE variables
  year <- catch <- NULL
  x_fy <- ggplot2::scale_x_continuous(
    breaks = scales::breaks_width(10),
    labels = function(x) paste(x, x + 1, sep = "/")
  )
  th <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(angle = 30, vjust = 0.5)
    )
  p1 <- ggplot2::ggplot(cpue, ggplot2::aes(year, catch)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = hr_label("fishing_year"), y = NULL, title = hr_label("catches_tonnes")) +
    x_fy +
    th
  p2 <- ggplot2::ggplot(cpue[!is.na(cpue$cpue), ], ggplot2::aes(year, cpue)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = hr_label("fishing_year"), y = NULL, title = hr_label("cpue_kg_h")) +
    x_fy +
    th
  patchwork::wrap_plots(p1, p2, nrow = 1)
}

#' @param advice_hist,tac_hist Advice and TAC history (\code{assessment_year}
#'   and \code{advice}, \code{tac}).
#' @param landings Data frame with \code{year} (first year of the fishing
#'   year) and \code{landings} (tonnes).
#' @return \subsection{hr_techreport_plot_advice_tac}{Advice, TAC and
#'   landings by fishing year (thousand tonnes)}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_advice_tac <- function(advice_hist, tac_hist, landings) {
  # NSE variables
  year <- value <- name <- assessment_year <- advice <- tac <- NULL
  d <- dplyr::bind_rows(
    dplyr::transmute(tac_hist, year = assessment_year, name = hr_label("tac"), value = tac),
    dplyr::transmute(advice_hist, year = assessment_year, name = hr_label("advice"), value = advice),
    dplyr::transmute(landings, year, name = hr_label("landings"), value = landings)
  ) |>
    dplyr::filter(year >= min(advice_hist$assessment_year), !is.na(value))
  ggplot2::ggplot(d, ggplot2::aes(year, value / 1000, colour = name, linetype = name)) +
    ggplot2::geom_line() +
    ggplot2::labs(y = hr_label("thousand_tonnes"), x = hr_label("fishing_year"), colour = NULL, linetype = NULL) +
    ggplot2::scale_x_continuous(
      breaks = scales::breaks_width(5),
      labels = function(x) paste(x, x + 1, sep = "/")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8),
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.8)
    )
}

#' @param temperature Data frame with \code{year}, \code{bottom} and
#'   \code{surface} (mean temperatures of the survey stations).
#' @return \subsection{hr_techreport_plot_survey_temperature}{Mean bottom
#'   (black) and surface (red) temperature by year}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_survey_temperature <- function(temperature) {
  # NSE variables
  year <- bottom <- surface <- NULL
  ggplot2::ggplot(temperature, ggplot2::aes(year)) +
    ggplot2::geom_line(ggplot2::aes(y = bottom)) +
    ggplot2::geom_point(ggplot2::aes(y = bottom)) +
    ggplot2::geom_line(ggplot2::aes(y = surface), col = "red") +
    ggplot2::geom_point(ggplot2::aes(y = surface), col = "red") +
    ggplot2::labs(x = hr_label("year"), y = hr_label("temperature_c")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8)
    )
}

#' @param bycatch_index Data frame with \code{species} (1 cod, 2 haddock, 4
#'   whiting), \code{size} (1 0-group, 2 older) and \code{year} and
#'   \code{index} (abundance).
#' @param species Species to show (codes).
#' @param scale Divisor of the index on the axis. Default 1e5, as the old
#'   shrimp reports.
#' @param sizes Size groups to show (1 0-group, 2 older).
#' @return \subsection{hr_techreport_plot_bycatch_index}{Bars of the
#'   abundance index by year, species (columns) and size (rows). If
#'   \code{bycatch_index} has a column \code{season} (e.g. autumn and
#'   February surveys), species are the rows and seasons the columns}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_bycatch_index <- function(bycatch_index, species = c(1, 2), scale = 1e5, sizes = c(1, 2)) {
  # NSE variables
  year <- index <- species_label <- size_label <- size <- NULL
  sp_keys <- c(`1` = "cod", `2` = "haddock", `4` = "whiting")
  sp <- species
  d <- bycatch_index[bycatch_index$species %in% sp & bycatch_index$size %in% sizes, ]
  sp_labels <- vapply(sp_keys[as.character(sp)], hr_label, character(1))
  d$species_label <- factor(
    vapply(sp_keys[as.character(d$species)], hr_label, character(1)),
    levels = sp_labels
  )
  d$size_label <- factor(
    ifelse(d$size == 1, hr_label("group_0"), hr_label("group_1plus")),
    levels = c(hr_label("group_0"), hr_label("group_1plus"))
  )
  ggplot2::ggplot(d, ggplot2::aes(year, index / scale)) +
    ggplot2::geom_col(width = 0.5) +
    (if ("season" %in% names(d)) {
      ggplot2::facet_grid(species_label ~ season, scales = "free_y")
    } else {
      ggplot2::facet_grid(size_label ~ species_label, scales = "free_y")
    }) +
    ggplot2::labs(x = NULL, y = hr_label("index")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8),
      strip.background = ggplot2::element_rect(fill = "white")
    )
}

#' @param landings Data frame with \code{year} and \code{landings} (tonnes).
#' @param vessels Data frame with \code{year} and \code{vessels} (number of
#'   vessels in the fishery).
#' @return \subsection{hr_techreport_plot_catch_vessels}{Two panels of bars:
#'   catch and the number of vessels by year}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_catch_vessels <- function(landings, vessels) {
  # NSE variables
  year <- NULL
  th <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8)
    )
  p1 <- ggplot2::ggplot(landings[!is.na(landings$landings), ], ggplot2::aes(year, landings)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = hr_label("year"), y = NULL, title = hr_label("catches_tonnes")) +
    th
  p2 <- ggplot2::ggplot(vessels, ggplot2::aes(year, vessels)) +
    ggplot2::geom_col() +
    ggplot2::labs(x = hr_label("year"), y = NULL, title = hr_label("number_of_vessels")) +
    th
  patchwork::wrap_plots(p1, p2, nrow = 1)
}

#' @param effects Data frame of model effects with \code{effect}
#'   (\code{"year"}, \code{"month"}, \code{"trawl_size"}), \code{level}
#'   (numeric), \code{mean}, \code{lower} and \code{upper} (exp of the
#'   estimate and of the estimate -/+ 2 standard errors), e.g. of a
#'   logbook CPUE GLM.
#' @return \subsection{hr_techreport_plot_cpue_effects}{One panel per effect:
#'   the standardised CPUE by year, month and trawl size}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_cpue_effects <- function(effects) {
  # NSE variables
  level <- lower <- upper <- NULL
  keys <- c(year = "year", month = "month", trawl_size = "trawl_size")
  panel <- function(e, ylab) {
    d <- effects[effects$effect == e, ]
    p <- ggplot2::ggplot(d, ggplot2::aes(level, mean)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::geom_linerange(ggplot2::aes(ymin = lower, ymax = upper)) +
      ggplot2::labs(x = hr_label(keys[[e]]), y = ylab) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = 8),
        text = ggplot2::element_text(size = 8)
      )
    if (e == "month") {
      p <- p + ggplot2::scale_x_continuous(breaks = 1:12)
    }
    p
  }
  es <- intersect(names(keys), unique(effects$effect))
  plots <- lapply(seq_along(es), function(k) {
    panel(es[k], if (k == 1) hr_label("standardized_cpue") else NULL)
  })
  patchwork::wrap_plots(plots, nrow = 1)
}

#' @param cpue Data frame with \code{year} and \code{mean} (the standardised
#'   CPUE by year).
#' @param index Data frame with \code{year} and \code{b} (the total biomass
#'   index), e.g. the total index of \code{hr_techreport_plot_survey_indices()}.
#' @return \subsection{hr_techreport_plot_cpue_index}{Two panels: the
#'   standardised CPUE against the total biomass index, a path through the
#'   years; and both by year, CPUE as a line (left axis) and the index as
#'   points on a dashed line (right axis)}
#' @rdname hr_techreport_plot_invertebrate
#' @export
hr_techreport_plot_cpue_index <- function(cpue, index) {
  # NSE variables
  year <- b <- NULL
  d <- dplyr::inner_join(cpue, index[, c("year", "b")], by = "year")
  th <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      text = ggplot2::element_text(size = 8)
    )
  p1 <- ggplot2::ggplot(d, ggplot2::aes(b, mean, colour = year, label = year)) +
    ggplot2::geom_path() +
    ggplot2::geom_point() +
    ggplot2::geom_text(hjust = 1, vjust = -1, size = 2) +
    ggplot2::scale_colour_viridis_c(name = hr_label("year")) +
    ggplot2::labs(x = hr_label("total_biomass_index"), y = hr_label("standardized_cpue")) +
    th +
    ggplot2::theme(legend.position = "inside", legend.position.inside = c(0.85, 0.25))
  k <- max(cpue$mean, na.rm = TRUE) / max(index$b, na.rm = TRUE)
  idx <- index[index$year >= min(cpue$year) & index$year <= max(cpue$year), ]
  p2 <- ggplot2::ggplot() +
    ggplot2::geom_line(data = cpue, ggplot2::aes(year, mean)) +
    ggplot2::geom_line(data = idx, ggplot2::aes(year, b * k), linetype = 2) +
    ggplot2::geom_point(data = idx, ggplot2::aes(year, b * k)) +
    ggplot2::scale_y_continuous(
      name = hr_label("standardized_cpue"),
      sec.axis = ggplot2::sec_axis(~ . / k, name = hr_label("total_biomass_index"))
    ) +
    ggplot2::labs(x = hr_label("year")) +
    th
  patchwork::wrap_plots(p1, p2, nrow = 1)
}
