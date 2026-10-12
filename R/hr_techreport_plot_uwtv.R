#' Tech report figures of an underwater TV (UWTV) survey and its fishery
#'
#' Figures of the Nephrops (Norway lobster) tech report, from the targets of
#' the 40-nep pipeline: the burrow index by area, station densities, kriged
#' density maps, the grounds, a bubble map (e.g. larval densities), survey
#' CPUE by area, landings, CPUE and effort by region, length distributions
#' by sex as ridges, and a simple time series (e.g. trawl marks). They take
#' plain data frames, so other video or camera surveys can use them.
#'
#' @param index_area Data frame with \code{year}, \code{area_name},
#'   \code{burrows_millions}, \code{low} and \code{high}.
#' @param point_years Years drawn as points with error bars instead of the
#'   line and band (the biennial years). Default none.
#' @param ncol Number of columns of panels.
#' @return A \code{ggplot2} object.
#' @name hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_uwtv_index_area <- function(index_area, point_years = NULL, ncol = 3) {
  # NSE variables
  year <- burrows_millions <- low <- high <- NULL
  d_line <- dplyr::filter(index_area, !(year %in% point_years))
  d_point <- dplyr::filter(index_area, year %in% point_years)
  ggplot2::ggplot(index_area, ggplot2::aes(year, burrows_millions)) +
    ggplot2::geom_ribbon(
      data = d_line,
      ggplot2::aes(ymin = low, ymax = high),
      fill = "darkgreen",
      alpha = 0.3
    ) +
    ggplot2::geom_line(data = d_line, col = "darkgreen", linewidth = 0.5) +
    ggplot2::geom_point(data = d_line, col = "darkgreen", size = 1) +
    ggplot2::geom_pointrange(
      data = d_point,
      ggplot2::aes(ymin = low, ymax = high),
      col = "darkgreen",
      size = 0.3
    ) +
    ggplot2::facet_wrap(~area_name, scales = "free_y", ncol = ncol) +
    ggplot2::labs(x = hr_label("year"), y = hr_label("abundance_millions")) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = c(0, 0.1)),
      limits = c(0, NA)
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(2)) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(size = 12),
      axis.text = ggplot2::element_text(size = 9),
      strip.background = ggplot2::element_rect(fill = "white")
    )
}

#' @param stations Data frame with \code{year} and \code{dens} (density at
#'   the stations).
#' @param years All years of the axis, so years without a survey show as
#'   gaps. Default the years of \code{stations}.
#' @return \subsection{hr_techreport_plot_station_density}{Violins and boxes
#'   of the station densities by year, with the mean as a line}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_station_density <- function(stations, years = NULL) {
  # NSE variables
  year <- dens <- NULL
  if (is.null(years)) {
    years <- sort(unique(stations$year))
  }
  d <- dplyr::bind_rows(
    dplyr::select(stations, year, dens),
    tibble::tibble(year = setdiff(years, stations$year), dens = NA_real_)
  )
  ggplot2::ggplot(d, ggplot2::aes(x = as.factor(year), y = dens)) +
    ggplot2::geom_violin(
      ggplot2::aes(group = year, colour = year, fill = year),
      alpha = 0.5,
      kernel = "rectangular",
      na.rm = TRUE
    ) +
    ggplot2::geom_boxplot(ggplot2::aes(group = year), width = 0.2, na.rm = TRUE) +
    ggplot2::stat_summary(
      fun = mean,
      geom = "line",
      colour = "blue",
      ggplot2::aes(group = 1),
      na.rm = TRUE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      text = ggplot2::element_text(size = 14),
      axis.text = ggplot2::element_text(size = 12)
    ) +
    ggplot2::labs(x = hr_label("year"), y = hr_label("density_n_m2"))
}

#' @param grid Data frame with \code{x}, \code{y} (longitude and latitude
#'   of the cells of a regular grid), \code{value} and, for several years,
#'   \code{year}.
#' @param polygons sf polygons drawn over the grid (e.g. the grounds).
#' @param points Optional data frame with \code{lon} and \code{lat} of the
#'   stations, drawn as small crosses.
#' @param limits Limits of the colour scale; values outside are squished.
#' @param fill_label Title of the colour legend.
#' @param xlim,ylim Map limits (longitude, latitude).
#' @param facet If \code{TRUE}, a panel for each \code{year}.
#' @return \subsection{hr_techreport_plot_grid_map}{A map of the grid values
#'   (e.g. kriged density or its variance)}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_grid_map <- function(
  grid,
  polygons,
  points = NULL,
  limits = c(0, 0.5),
  fill_label = hr_label("density_n_m2"),
  xlim = c(-25, -13.7),
  ylim = c(63, 64.9),
  facet = FALSE,
  ncol = 2
) {
  # NSE variables
  x <- y <- value <- lon <- lat <- long <- group <- NULL
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = grid, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_polygon(
      data = ggplot2::map_data("mapdata::worldHires", "Iceland"),
      ggplot2::aes(long, lat, group = group),
      fill = "grey80",
      colour = "black",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(data = polygons, fill = NA, colour = "black", linewidth = 0.2) +
    ggplot2::scale_fill_viridis_c(
      name = fill_label,
      limits = limits,
      oob = scales::squish,
      na.value = NA
    )
  if (!is.null(points)) {
    p <- p +
      ggplot2::geom_point(
        data = points,
        ggplot2::aes(x = lon, y = lat),
        shape = 4,
        size = 0.005,
        colour = "darkgrey"
      )
  }
  if (isTRUE(facet)) {
    p <- p + ggplot2::facet_wrap(~year, ncol = ncol)
  }
  p +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white"),
      strip.text = ggplot2::element_text(size = 8, margin = ggplot2::margin(2, 0, 2, 0))
    )
}

#' @param label_nudge Data frame with \code{name}, \code{nudge_x} and
#'   \code{nudge_y}: where to put the label of each polygon (column
#'   \code{name} of \code{polygons}) relative to its centroid. Polygons not
#'   listed are labelled at the centroid.
#' @param fill Fill colour of the polygons.
#' @return \subsection{hr_techreport_plot_polygon_map}{A map of the polygons
#'   (e.g. the grounds) with their names}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_polygon_map <- function(
  polygons,
  label_nudge = NULL,
  fill = "lightblue",
  xlim = c(-25, -13.7),
  ylim = c(63, 64.9)
) {
  # NSE variables
  long <- lat <- group <- name <- NULL
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = ggplot2::map_data("mapdata::worldHires", "Iceland"),
      ggplot2::aes(long, lat, group = group),
      fill = "grey80",
      colour = "black",
      linewidth = 0.2
    ) +
    ggplot2::geom_sf(data = polygons, fill = fill, colour = "black", linewidth = 0.2)
  for (i in seq_len(nrow(polygons))) {
    n <- polygons$name[i]
    nudge <- if (!is.null(label_nudge) && n %in% label_nudge$name) {
      label_nudge[label_nudge$name == n, ]
    } else {
      data.frame(nudge_x = 0, nudge_y = 0)
    }
    p <- p +
      ggplot2::geom_sf_text(
        data = polygons[i, ],
        ggplot2::aes(label = name),
        nudge_x = nudge$nudge_x,
        nudge_y = nudge$nudge_y,
        size = 3.5
      )
  }
  p +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank()
    )
}

#' @param bubbles Data frame with \code{year}, \code{longitude},
#'   \code{latitude} and the value to show (column \code{value_col}); zeros
#'   are drawn as crosses.
#' @param value_col Column of the bubble size and colour, e.g.
#'   \code{"density_total"}.
#' @param size_label Title of the legend.
#' @return \subsection{hr_techreport_plot_bubble_map}{Maps by year of the
#'   values at the stations as bubbles, zeros as crosses (e.g. larval
#'   densities)}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_bubble_map <- function(
  bubbles,
  value_col = "density_total",
  size_label = hr_label("larval_density"),
  xlim = c(-25, -13.7),
  ylim = c(63, 64.9),
  ncol = 2
) {
  # NSE variables
  longitude <- latitude <- value <- long <- lat <- group <- .data <- NULL
  d <- dplyr::mutate(bubbles, value = .data[[value_col]])
  zeros <- dplyr::filter(d, value == 0 | is.na(value))
  nonzeros <- dplyr::filter(d, value > 0)
  max_v <- max(nonzeros$value, na.rm = TRUE)
  brks <- pretty(c(0, max_v), n = 4)
  brks <- sort(unique(c(0, brks[brks >= 0 & brks <= max_v])))
  n_brks <- length(brks)
  cols <- scales::viridis_pal(option = "C")(n_brks)
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = ggplot2::map_data("world", region = "Iceland"),
      ggplot2::aes(x = long, y = lat, group = group),
      fill = "grey80",
      colour = "black",
      linewidth = 0.2
    ) +
    ggplot2::geom_point(
      data = zeros,
      ggplot2::aes(x = longitude, y = latitude),
      shape = 4,
      size = 1.5,
      colour = "black",
      stroke = 0.5
    ) +
    ggplot2::geom_point(
      data = nonzeros,
      ggplot2::aes(x = longitude, y = latitude, size = value, fill = value),
      shape = 21,
      colour = "black",
      stroke = 0.3,
      alpha = 0.6
    ) +
    # An invisible layer with all the data drives the combined legend
    ggplot2::geom_point(
      data = d,
      ggplot2::aes(x = longitude, y = latitude, size = value, fill = value),
      shape = NA
    ) +
    ggplot2::facet_wrap(~year, ncol = ncol) +
    ggplot2::coord_quickmap(xlim = xlim, ylim = ylim) +
    ggplot2::scale_size_continuous(
      range = c(1.5, 12),
      name = size_label,
      breaks = brks,
      limits = c(0, max_v)
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      name = size_label,
      breaks = brks,
      limits = c(0, max_v)
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(),
      size = ggplot2::guide_legend(
        override.aes = list(
          shape = c(4, rep(21, n_brks - 1)),
          fill = c(NA, cols[-1]),
          colour = rep("black", n_brks),
          stroke = c(0.5, rep(0.3, n_brks - 1)),
          alpha = c(1, rep(0.6, n_brks - 1))
        )
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "right",
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(size = 8, margin = ggplot2::margin(2, 0, 2, 0)),
      panel.grid.major = ggplot2::element_blank()
    )
}

#' @param survey_cpue Data frame with \code{year}, \code{area_name} and
#'   \code{cpue}; rows without an area are left out.
#' @return \subsection{hr_techreport_plot_cpue_area}{Survey CPUE by area and
#'   year, a panel for each area}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_cpue_area <- function(survey_cpue) {
  # NSE variables
  area_name <- year <- cpue <- NULL
  d <- survey_cpue |>
    dplyr::filter(!is.na(area_name)) |>
    tidyr::complete(area_name, year = min(year):max(year))
  ggplot2::ggplot(d, ggplot2::aes(x = year, y = cpue)) +
    ggplot2::geom_line(na.rm = TRUE) +
    ggplot2::geom_point(size = 1, na.rm = TRUE) +
    ggplot2::facet_wrap(~area_name) +
    ggplot2::labs(x = hr_label("year"), y = hr_label("cpue")) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )
}

#' @param landings_region Data frame with \code{year}, \code{area} and
#'   \code{catch} (tonnes) and, for the CPUE and effort figures,
#'   \code{cpue}. The area \code{"total"} is left out.
#' @param area_labels Named character vector of labels by area. Areas
#'   \code{"iceland"} and \code{"other"} (the years before the regions)
#'   are labelled Iceland and other countries.
#' @param region_start First year by region: earlier years are shown as
#'   Iceland and other countries.
#' @return \subsection{hr_techreport_plot_landings_region}{Landings (kt) by
#'   region (or country before \code{region_start}) as stacked bars}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_landings_region <- function(
  landings_region,
  area_labels,
  region_start = 1962
) {
  # NSE variables
  area <- year <- catch <- fill_var <- fill_label <- NULL
  d <- landings_region |>
    dplyr::filter(area != "total") |>
    dplyr::mutate(
      fill_var = dplyr::case_when(
        year < region_start & area == "iceland" ~ "iceland",
        year < region_start ~ "other",
        TRUE ~ area
      ),
      fill_label = dplyr::recode(
        fill_var,
        !!!as.list(area_labels),
        iceland = hr_label("iceland"),
        other = hr_label("other")
      )
    ) |>
    dplyr::group_by(year, fill_label) |>
    dplyr::summarize(catch = sum(catch, na.rm = TRUE), .groups = "drop")
  ggplot2::ggplot(d, ggplot2::aes(x = year, y = catch / 1000, fill = fill_label)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::theme_bw() +
    ggplot2::labs(y = hr_label("landings_thousand_t"), x = hr_label("year"), fill = "") +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = "inside",
      legend.position.inside = c(0.85, 0.85)
    ) +
    ggplot2::scale_x_continuous(breaks = scales::breaks_width(10)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    hr_theme_crayola_fill()
}

#' @param what \code{"cpue"} or \code{"effort"} (catch / CPUE).
#' @param year_start First year shown (after it).
#' @return \subsection{hr_techreport_plot_region_series}{CPUE or effort by
#'   region as lines}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_region_series <- function(
  landings_region,
  area_labels,
  what = c("cpue", "effort"),
  year_start = 1970
) {
  # NSE variables
  area <- year <- cpue <- catch <- value <- NULL
  what <- match.arg(what)
  d <- landings_region |>
    dplyr::filter(year > year_start, area != "total", !is.na(cpue)) |>
    dplyr::mutate(value = if (what == "cpue") cpue else catch / cpue)
  ggplot2::ggplot(d) +
    ggplot2::geom_line(ggplot2::aes(x = year, y = value, col = area), linewidth = 1.2) +
    # Colours 3, 4, 5... of the crayola (Paired) palette, as the old figures
    ggplot2::scale_colour_manual(
      values = stats::setNames(
        hr_theme_crayola_fill()$palette(1)[2 + seq_along(area_labels)],
        names(area_labels)
      ),
      labels = area_labels
    ) +
    ggplot2::labs(
      x = hr_label("year"),
      y = hr_label(if (what == "cpue") "cpue" else "effort")
    ) +
    ggplot2::theme_light() +
    ggplot2::theme(
      legend.position = "inside",
      legend.position.inside = if (what == "cpue") c(0.3, 0.7) else c(0.5, 0.8),
      legend.title = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}

#' @param ldist Data frame with \code{year}, \code{length} (cm), \code{sex}
#'   (factor \code{"Male"}, \code{"Female"}) and \code{number} (count).
#' @param year_breaks Years labelled on the axis.
#' @return \subsection{hr_techreport_plot_ldist_ridges}{Length distributions
#'   (mm) by year and sex as density ridges with the quartiles}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_ldist_ridges <- function(
  ldist,
  year_breaks = seq(1970, 2025, 5),
  xlim = c(15, 85)
) {
  # NSE variables
  length <- year <- sex <- sex_label <- number <- NULL
  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("hr_techreport_plot_ldist_ridges() needs the ggridges package")
  }
  sex_labels <- c(Male = hr_label("male"), Female = hr_label("female"))
  d <- ldist |>
    dplyr::select(year, length, sex, number) |>
    tidyr::uncount(number) |>
    dplyr::mutate(sex_label = factor(sex_labels[as.character(sex)], levels = sex_labels))
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = length * 10, y = forcats::fct_rev(factor(year)), fill = sex)
  ) +
    ggridges::stat_density_ridges(
      alpha = 0.7,
      scale = 1.5,
      rel_min_height = 0.01,
      color = "black",
      linewidth = 0.3,
      quantile_lines = TRUE,
      quantiles = c(0.25, 0.5, 0.75)
    ) +
    ggplot2::scale_fill_manual(
      values = c(Male = "steelblue", Female = "salmon"),
      name = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = seq(20, 80, 10), limits = xlim) +
    ggplot2::scale_y_discrete(expand = c(0.01, 0), breaks = year_breaks) +
    ggplot2::facet_wrap(~sex_label) +
    ggplot2::labs(x = hr_label("carapace_length_mm"), y = NULL) +
    ggridges::theme_ridges() +
    ggplot2::theme(
      legend.position = "none",
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      strip.background = ggplot2::element_rect(fill = "grey90"),
      axis.text.y = ggplot2::element_text(size = 8),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 12, hjust = 0.5),
      panel.spacing = ggplot2::unit(1, "lines")
    )
}

#' @param series Data frame with \code{year} and \code{value}.
#' @param y_label Axis label.
#' @return \subsection{hr_techreport_plot_series}{A time series as a line
#'   with points (e.g. trawl marks on the video transects)}
#' @rdname hr_techreport_plot_uwtv
#' @export
hr_techreport_plot_series <- function(series, y_label = "") {
  # NSE variables
  year <- value <- NULL
  ggplot2::ggplot(series, ggplot2::aes(x = year, y = value)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::labs(x = hr_label("year"), y = y_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank()
    )
}
