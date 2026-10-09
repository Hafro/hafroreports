#' Tech report figures of the rfb rule
#'
#' The four figures of the category 3 (rfb rule) tech reports, as the old
#' \code{dls_iplotter()}, \code{dls_Lplotter()}, \code{dls_MLplotter()} and
#' \code{dls_Fplotter()} that the stock repositories each kept a copy of:
#' \itemize{
#'   \item \code{hr_techreport_plot_rfb_index()}: the biomass index of the
#'     rule with its 95\% interval, the means of index A (last two years) and
#'     index B (the three years before) in red, I_trigger (dashed) and I_loss
#'     (a point at the lowest index, or a line);
#'   \item \code{hr_techreport_plot_rfb_lc()}: the length distribution of
#'     the catch over all years, with L_c and L_F=M (red bars), 50\% of the
#'     modal abundance (dashed) and L_inf (dashed);
#'   \item \code{hr_techreport_plot_rfb_ml()}: the length distribution of the
#'     catch in the last data year, with L_F=M and the mean length above L_c;
#'   \item \code{hr_techreport_plot_rfb_f()}: the fishing pressure proxy
#'     L_F=M / L_mean by year, with the F_MSY proxy (1).
#' }
#' Labels follow \code{getOption("hr.lang")} through \code{\link{hr_label}}.
#'
#' @param survey_index Survey indices with columns \code{index}, \code{year},
#'   \code{b} (tonnes) and \code{b_cv}.
#' @param index Name of the index of the rule in \code{survey_index}.
#' @param rfb_prognosis The rule's components (columns \code{component} and
#'   \code{value}), as \code{dlsrules::rfb_advice()}: \code{index_A} and
#'   \code{index_B} are drawn, and for \code{hr_techreport_plot_rfb_ml()}
#'   (if given) \code{mean_catch_length}.
#' @param ref_points Named list of reference points: \code{I_trigger},
#'   \code{I_lim} (I_loss); \code{Lc}, \code{Linf},
#'   \code{target_reference_length} (L_F=M, or \code{trl}) and
#'   \code{F_msy_proxy} (default 1).
#' @param assessment_year The assessment year: the index is drawn up to it.
#' @param iloss How I_loss is shown: \code{"point"} (default), a point at
#'   the lowest index (or \code{iloss_year}); or \code{"line"}, a line at
#'   \code{ref_points$I_lim} (when I_loss is not an index value, e.g. a mean
#'   of years).
#' @param iloss_year Year of the I_loss point. Default
#'   \code{ref_points$I_lim_year} (as \code{dlsrules::ref_points_iloss()}),
#'   or else the year of the lowest index up to \code{assessment_year}.
#' @param iloss_label Plotmath label of I_loss. Default \code{"I[loss]"}.
#' @param label_x Named numeric vector, the years at which the
#'   \code{trigger} and \code{iloss} labels are written. Default: the year
#'   of I_loss (\code{iloss_year}), less one year for the I_trigger label.
#' @param label_y Named numeric vector, the heights of the \code{trigger}
#'   and \code{iloss} labels as multiples of their values. Default
#'   \code{c(trigger = 1.2, iloss = 0.75)}.
#' @param unit Divisor of the index for the y axis: 1000 (default, thousand
#'   tonnes) or 1 (tonnes).
#' @param ci Draw the 95\% interval (log-normal, from \code{b_cv}). Default
#'   \code{TRUE}.
#' @param points Also draw the yearly values as points. Default
#'   \code{FALSE}.
#' @param title Plot title. Default \code{hr_label("rfb_biomass_index")}.
#' @param base_size Base text size (ggplot2 text size units).
#' @return A \code{ggplot2} plot object.
#' @name hr_techreport_plot_rfb
NULL

#' @describeIn hr_techreport_plot_rfb Theme of the rfb figures: axis text
#'   \code{2 * base_size}, bold axis titles and title \code{3 * base_size}.
#' @export
hr_techreport_rfb_theme <- function(base_size = 3) {
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = base_size * 2),
    axis.title = ggplot2::element_text(size = base_size * 3, face = "bold"),
    plot.title = ggplot2::element_text(size = base_size * 3, face = "bold")
  )
}

#' @describeIn hr_techreport_plot_rfb The biomass index of the rule.
#' @export
hr_techreport_plot_rfb_index <- function(
  survey_index,
  index,
  rfb_prognosis,
  ref_points,
  assessment_year,
  iloss = c("point", "line"),
  iloss_year = NULL,
  iloss_label = "I[loss]",
  label_x = NULL,
  label_y = c(trigger = 1.2, iloss = 0.75),
  unit = 1e3,
  ci = TRUE,
  points = FALSE,
  x_limits = NULL,
  title = NULL,
  base_size = 3
) {
  # NSE variables
  year <- b <- b_cv <- lower <- upper <- value <- group <- .data <- NULL
  iloss <- match.arg(iloss)

  idata <- survey_index |>
    dplyr::filter(.data$index == .env$index, year <= .env$assessment_year) |>
    dplyr::mutate(
      lower = b * exp(-1.96 * b_cv),
      upper = b * exp(1.96 * b_cv)
    )
  if (is.null(iloss_year)) {
    iloss_year <- ref_points$I_lim_year
  }
  if (length(iloss_year) != 1 || is.na(iloss_year)) {
    iloss_year <- idata$year[which.min(idata$b)]
  }
  ipoint <- idata[idata$year == iloss_year, ]
  label_x <- rfb_merge(c(trigger = iloss_year - 1, iloss = iloss_year), label_x)
  label_y <- rfb_merge(c(trigger = 1.2, iloss = 0.75), label_y)
  iloss_value <- if (iloss == "point") ipoint$b else ref_points$I_lim
  avg <- rfb_index_ab(rfb_prognosis, assessment_year)

  p <- ggplot2::ggplot(idata, ggplot2::aes(year, b / unit)) +
    ggplot2::theme_light()
  if (isTRUE(ci)) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = lower / unit, ymax = upper / unit),
        alpha = 0.5,
        fill = "lightblue"
      )
  }
  p <- p +
    ggplot2::geom_hline(yintercept = ref_points$I_trigger / unit, lty = 2) +
    (if (iloss == "line") {
      ggplot2::geom_hline(yintercept = ref_points$I_lim / unit)
    } else {
      list()
    }) +
    ggplot2::annotate(
      "text",
      x = label_x[["trigger"]],
      y = ref_points$I_trigger * label_y[["trigger"]] / unit,
      label = "I[trigger]",
      size = base_size,
      parse = TRUE
    ) +
    ggplot2::geom_line(color = "blue") +
    (if (isTRUE(points)) ggplot2::geom_point(color = "blue") else list()) +
    ggplot2::geom_line(
      data = avg,
      ggplot2::aes(year, value / unit, group = group),
      color = "red",
      linewidth = 0.5
    ) +
    (if (iloss == "point") {
      ggplot2::geom_point(data = ipoint, col = "black", pch = 18, size = 3)
    } else {
      list()
    }) +
    ggplot2::annotate(
      "text",
      x = label_x[["iloss"]],
      y = iloss_value * label_y[["iloss"]] / unit,
      label = iloss_label,
      size = base_size,
      parse = TRUE
    ) +
    (if (is.null(x_limits)) {
      ggplot2::expand_limits(y = 0)
    } else {
      ggplot2::expand_limits(x = x_limits, y = 0)
    }) +
    hr_techreport_rfb_theme(base_size) +
    ggplot2::labs(
      x = hr_label("year"),
      y = hr_label(if (unit == 1e3) "thousand_tonnes" else "tonnes"),
      title = if (is.null(title)) hr_label("rfb_biomass_index") else title
    )
  p
}

#' @describeIn hr_techreport_plot_rfb The length distribution of the catch
#'   over all years with the length-based reference points.
#' @param ldist Length distributions of the catch of the rule, with columns
#'   \code{year}, \code{length} (cm) and \code{n}.
#' @param x_limits Lengths (cm) or years the x axis spans at least, or
#'   \code{NULL}.
#' @export
hr_techreport_plot_rfb_lc <- function(
  ldist,
  ref_points,
  x_limits = c(0, 70),
  base_size = 3
) {
  # NSE variables
  n <- NULL

  ldist_all <- ldist |>
    dplyr::group_by(length) |>
    dplyr::summarise(n = sum(n), .groups = "drop")
  n_max <- max(ldist_all$n)
  trl <- rfb_trl(ref_points)

  ggplot2::ggplot(ldist_all, ggplot2::aes(length, n)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::geom_col(
      data = ldist_all |>
        dplyr::filter(length %in% c(ref_points$Lc, round(trl))),
      fill = "red",
      width = 0.5
    ) +
    ggplot2::annotate(
      "text",
      x = c(ref_points$Lc, trl),
      y = -0.02 * n_max,
      label = c("L[c]", "L[F==M]"),
      parse = TRUE,
      size = base_size
    ) +
    ggplot2::geom_vline(xintercept = ref_points$Linf, lty = 2) +
    ggplot2::geom_hline(yintercept = 0.5 * n_max, lty = 2) +
    ggplot2::annotate(
      "text",
      x = ref_points$Lc * 0.5,
      y = n_max * 0.54,
      label = hr_label("modal_abundance_50"),
      fontface = "bold",
      size = base_size * 0.6
    ) +
    ggplot2::annotate(
      "text",
      x = ref_points$Linf * 0.95,
      y = n_max * 0.9,
      label = "L[infinity]",
      parse = TRUE,
      size = base_size
    ) +
    ggplot2::expand_limits(x = x_limits, y = 0) +
    ggplot2::theme_light() +
    hr_techreport_rfb_theme(base_size) +
    ggplot2::labs(
      x = hr_label("length_cm"),
      y = hr_label("frequency"),
      title = hr_label("length_based_ref_points")
    )
}

#' @describeIn hr_techreport_plot_rfb The length distribution of the catch
#'   in \code{data_year} with L_F=M (solid) and the mean length above L_c
#'   (dashed). The mean length is \code{mean_catch_length} of
#'   \code{rfb_prognosis} if given (the rule's value), otherwise computed
#'   from \code{ldist}.
#' @param data_year The catch year of the rule (the year before the
#'   assessment year).
#' @param legend_position Legend position inside the panel. Default
#'   \code{c(0.8, 0.8)}.
#' @export
hr_techreport_plot_rfb_ml <- function(
  ldist,
  ref_points,
  data_year,
  rfb_prognosis = NULL,
  x_limits = c(0, 70),
  legend_position = c(0.8, 0.8),
  base_size = 3
) {
  # NSE variables
  year <- n <- x <- y <- rp <- NULL

  ld <- ldist |> dplyr::filter(year == data_year)
  lmean <- if (is.null(rfb_prognosis)) {
    rfb_mean_length(ld, ref_points$Lc)$L
  } else {
    rfb_prognosis$value[rfb_prognosis$component == "mean_catch_length"]
  }
  lines <- tibble::tibble(
    x = rep(c(rfb_trl(ref_points), lmean), each = 2),
    y = rep(c(0, max(ld$n)), 2),
    rp = rep(c("a", "b"), each = 2)
  )

  ggplot2::ggplot(ld, ggplot2::aes(length, n)) +
    ggplot2::geom_col(width = 0.5) +
    ggplot2::geom_line(
      data = lines,
      ggplot2::aes(x, y, linetype = rp),
      col = "red",
      linewidth = 0.5
    ) +
    ggplot2::theme_light() +
    ggplot2::expand_limits(x = x_limits, y = 0) +
    ggplot2::scale_linetype_discrete(
      labels = c(expression(L["F=M"]), expression(L["mean"]))
    ) +
    hr_techreport_rfb_theme(base_size) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = base_size * 3)
    ) +
    ggplot2::labs(
      x = hr_label("length_cm"),
      y = hr_label("frequency"),
      title = sprintf(hr_label("length_distribution_year"), data_year)
    )
}

#' @describeIn hr_techreport_plot_rfb The fishing pressure proxy L_F=M /
#'   L_mean by year (mean length above L_c of \code{ldist}, years
#'   \code{year_start} to \code{year_end}).
#' @param year_start,year_end First and last catch year of the series.
#' @param y_limits \code{NULL} (default) for the default axis; otherwise
#'   \code{c(lower, upper)}, the least range of the y axis, widened to the
#'   values (rounded out to 0.1), with breaks every 0.1.
#' @export
hr_techreport_plot_rfb_f <- function(
  ldist,
  ref_points,
  year_start = -Inf,
  year_end = Inf,
  x_limits = NULL,
  y_limits = NULL,
  base_size = 3
) {
  # NSE variables
  year <- L <- fpp <- NULL

  d <- ldist |>
    dplyr::filter(year >= year_start, year <= year_end) |>
    rfb_mean_length(ref_points$Lc) |>
    dplyr::mutate(fpp = rfb_trl(ref_points) / L)
  fmsy <- if (is.null(ref_points$F_msy_proxy)) 1 else ref_points$F_msy_proxy

  p <- ggplot2::ggplot(d, ggplot2::aes(year, fpp)) +
    ggplot2::geom_point(col = "red") +
    ggplot2::geom_line(col = "red") +
    ggplot2::geom_hline(yintercept = fmsy, linetype = "dashed", col = "black") +
    ggplot2::theme_light() +
    ggplot2::labs(
      x = hr_label("year"),
      y = expression(L["F=M"] / L["mean"]),
      title = hr_label("fproxy")
    ) +
    hr_techreport_rfb_theme(base_size)
  if (!is.null(y_limits)) {
    limits <- fproxy_axis_limits(d$fpp, y_limits = y_limits)
    p <- p +
      ggplot2::scale_y_continuous(
        breaks = seq(limits[1], limits[2], 0.1),
        expand = c(0, 0),
        limits = limits
      )
  }
  if (!is.null(x_limits)) {
    p <- p + ggplot2::expand_limits(x = x_limits)
  }
  p
}

#' Length-based reference point basis of a DSL figure
#'
#' L_c (the first length with more than 50\% of the modal abundance of the
#' catch), the largest length and the 99th percentile of the survey lengths,
#' L_inf (their mean) and L_F=M = 0.75 L_c + 0.25 L_inf, as the old
#' \code{R/04-DSL.R} of Norway redfish; the input of
#' \code{\link{hr_techreport_plot_dsl_lc}} and
#' \code{\link{hr_techreport_plot_dsl_ml}}.
#'
#' @param ldist Length distributions of the catch (columns \code{length},
#'   \code{n}; summed over years).
#' @param survey_ldist Length distributions of the survey (columns
#'   \code{length}, \code{n}).
#' @return A list with \code{ldist_all} (the catch by length),
#'   \code{modal_abun50}, \code{Lc}, \code{q99}, \code{max_len}, \code{Linf}
#'   and \code{LF}.
#' @export
hr_techreport_dsl_basis <- function(ldist, survey_ldist) {
  # NSE variables
  n <- NULL

  ldist_all <- ldist |>
    dplyr::group_by(length) |>
    dplyr::summarise(n = sum(n), .groups = "drop")
  modal_abun50 <- max(ldist_all$n) * 0.5
  Lc <- min(ldist_all$length[ldist_all$n > modal_abun50])
  survey_all <- survey_ldist |>
    dplyr::group_by(length) |>
    dplyr::summarise(n = sum(n), .groups = "drop")
  q99 <- unname(stats::quantile(rep(survey_all$length, round(survey_all$n)), 0.99))
  max_len <- max(survey_all$length)
  linf <- mean(c(max_len, q99))
  list(
    ldist_all = ldist_all,
    modal_abun50 = modal_abun50,
    Lc = Lc,
    q99 = q99,
    max_len = max_len,
    Linf = linf,
    LF = Lc * 0.75 + linf * 0.25
  )
}

#' Length frequency of the catch with the DSL reference points
#'
#' The length distribution of the catch with L_c (red bar), 50\% of the
#' modal abundance, L_inf, the largest length, the 99th percentile of the
#' survey lengths and L_F=M (coloured lines), as the old \code{R/04-DSL.R}
#' of Norway redfish. Labels follow \code{getOption("hr.lang")}.
#'
#' @param basis As \code{\link{hr_techreport_dsl_basis}}.
#' @param x_limits Lengths (cm) shown. Default \code{c(0, 45)}.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_dsl_lc <- function(basis, x_limits = c(0, 45)) {
  # NSE variables
  n <- NULL

  b <- basis
  ggplot2::ggplot(b$ldist_all, ggplot2::aes(length, n)) +
    ggplot2::geom_col() +
    ggplot2::geom_col(data = b$ldist_all |> dplyr::filter(length == b$Lc), fill = "red") +
    ggplot2::annotate("text", x = b$Lc, y = b$modal_abun50 * (-.1), label = "L[c]", size = 4, color = "red", parse = TRUE) +
    ggplot2::geom_hline(yintercept = b$modal_abun50, lty = 2) +
    ggplot2::annotate(
      "text",
      x = b$Lc / 2,
      y = b$modal_abun50 * 1.05,
      label = hr_label("modal_abundance_50_of"),
      size = 4
    ) +
    ggplot2::geom_vline(xintercept = b$Linf, col = "blue", linewidth = 1) +
    ggplot2::annotate("text", x = b$Linf * 0.98, y = b$modal_abun50 * 1.5, label = "L[infinity]", size = 4, parse = TRUE, angle = 90) +
    ggplot2::geom_vline(xintercept = b$max_len, col = "orange", linewidth = 1) +
    ggplot2::annotate(
      "text",
      x = b$max_len * 0.98,
      y = b$modal_abun50 * 1.5,
      label = hr_label("max_length"),
      size = 4,
      parse = TRUE,
      angle = 90
    ) +
    ggplot2::geom_vline(xintercept = b$q99, col = "yellow", linewidth = 1) +
    ggplot2::annotate(
      "text",
      x = b$q99 * 0.97,
      y = b$modal_abun50 * 1.5,
      label = hr_label("quantile_99"),
      size = 4,
      parse = TRUE,
      angle = 90
    ) +
    ggplot2::geom_vline(xintercept = b$LF, col = "green", linewidth = 1) +
    ggplot2::annotate("text", x = b$LF * 0.96, y = b$modal_abun50 * 1.9, label = "L[F == M]", size = 4, parse = TRUE, angle = 90) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = hr_label("length_cm"), y = hr_label("number")) +
    ggplot2::coord_cartesian(xlim = x_limits)
}

#' Mean length of the catch by year with L_F=M
#'
#' The mean length above L_c of the catch by year, with L_F=M (dashed), as
#' the old \code{R/04-DSL.R} of Norway redfish. Labels follow
#' \code{getOption("hr.lang")}.
#'
#' @param ldist Length distributions of the catch by year (columns
#'   \code{year}, \code{length}, \code{n}).
#' @param basis As \code{\link{hr_techreport_dsl_basis}} (\code{Lc},
#'   \code{LF}).
#' @param year_start,year_end Years on the x axis.
#' @param y_limits Fixed y axis \code{c(lower, upper)} (cm, breaks every
#'   cm), or \code{NULL} (default) for the default axis.
#' @param label_x Year of the L_F=M label. Default \code{year_end - 9}.
#' @return A \code{ggplot2} plot object.
#' @export
hr_techreport_plot_dsl_ml <- function(
  ldist,
  basis,
  year_start,
  year_end,
  y_limits = NULL,
  label_x = year_end - 9
) {
  # NSE variables
  year <- n <- L <- NULL

  ml <- ldist |>
    dplyr::filter(length > basis$Lc) |>
    dplyr::group_by(year) |>
    dplyr::summarise(L = sum(length * n) / sum(n), .groups = "drop")
  breaks <- c(
    year_start,
    seq(ceiling((year_start + 1) / 5) * 5, year_end - 3, 5),
    year_end
  )
  p <- tibble::tibble(year = year_start:year_end) |>
    dplyr::left_join(ml, by = "year") |>
    ggplot2::ggplot(ggplot2::aes(year, L)) +
    ggplot2::geom_point(color = "red", size = 1) +
    ggplot2::geom_line(linewidth = 0.5, color = "red") +
    ggplot2::geom_hline(yintercept = basis$LF, lty = 2, color = "blue") +
    ggplot2::annotate("text", x = label_x, y = basis$LF * 1.01, label = "L[F == M]", size = 5.5, parse = TRUE) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      y = hr_label("mean_length_catch_cm"),
      x = hr_label("year")
    )
  if (!is.null(y_limits)) {
    p <- p +
      ggplot2::scale_y_continuous(
        breaks = seq(y_limits[1], y_limits[2], 1),
        expand = c(0, 0),
        limits = y_limits
      )
  }
  p +
    ggplot2::coord_cartesian(xlim = c(year_start - 1, year_end + 1)) +
    ggplot2::scale_x_continuous(breaks = breaks, expand = c(0, 0.5))
}

# Mean length above Lc by year
rfb_mean_length <- function(ldist, Lc) {
  # NSE variables
  year <- n <- NULL

  ldist |>
    dplyr::filter(length > Lc) |>
    dplyr::group_by(year) |>
    dplyr::summarise(L = sum(length * n) / sum(n), .groups = "drop")
}

# L_F=M of the reference points (target_reference_length, or trl)
rfb_trl <- function(ref_points) {
  out <- ref_points$target_reference_length
  if (is.null(out)) out <- ref_points$trl
  if (is.null(out)) stop("ref_points needs target_reference_length (L_F=M)")
  out
}

# Index A (the last two years, from assessment_year - 1.5) and index B (the
# three years before) of the rule as lines
rfb_index_ab <- function(rfb_prognosis, assessment_year) {
  rfb <- function(comp) {
    rfb_prognosis$value[rfb_prognosis$component == comp]
  }
  dplyr::bind_rows(
    tibble::tibble(
      year = c(assessment_year - 1.5, assessment_year),
      value = rfb("index_A"),
      group = "A"
    ),
    tibble::tibble(
      year = c(assessment_year - 4.5, assessment_year - 1.5),
      value = rfb("index_B"),
      group = "B"
    )
  )
}

# Defaults overridden by the named elements of x
rfb_merge <- function(default, x) {
  if (is.null(x)) return(default)
  default[names(x)] <- x
  default
}
