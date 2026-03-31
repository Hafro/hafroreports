hr_techreport_plot_muppetfit_selection <- function(
  fit,
  assessment_year
) {
  # NSE variables
  variable <- model <- w <- sel <- index <- value <- year <- age <- filename <- NULL
  survey <- sigma <- name <- std.dev <- selslope <- fullselwt <- m <- l <- u <- NULL
  SigmaC <- SigmaSurvey1 <- SigmaSurvey2 <- SurveylnQ1 <- SurveylnQ2 <- NULL
  Spawningstock <- Recruitment <- NULL

  selection_plot_1 <-
    fit$params |>
    dplyr::filter(
      variable %in% c('fullselwt', 'selslope'),
      model == 'logit_length'
    ) |>
    dplyr::select(-c(name, index, std.dev)) |>
    tidyr::spread(variable, value) |>
    dplyr::left_join(tibble::tibble(
      model = 'logit_length',
      w = seq(0, 6000, by = 100)
    )) |>
    dplyr::mutate(sel = 1 / (1 + exp(-selslope * log(w / fullselwt)))) |>
    ggplot2::ggplot(ggplot2::aes(w, sel)) +
    ggplot2::geom_ribbon(
      data = fit$mcmc_results |>
        dplyr::filter(variable %in% c('fullselwt', 'selslope')) |>
        dplyr::select(-year) |>
        tidyr::spread(variable, value) |>
        dplyr::left_join(tibble::tibble(
          filename = 'parameter',
          w = seq(0, 6000, by = 100)
        )) |>
        dplyr::mutate(sel = 1 / (1 + exp(-selslope * log(w / fullselwt)))) |>
        dplyr::group_by(w) |>
        dplyr::summarise(
          m = median(sel),
          l = quantile(sel, 0.025),
          u = quantile(sel, 0.975)
        ),
      ggplot2::aes(y = m, ymin = l, ymax = u),
      fill = 'gold'
    ) +
    ggplot2::geom_line() +
    ggplot2::labs(y = 'Selection', x = 'Stock weight (g)')

  selection_plot_2 <-
    fit$rbage |>
    dplyr::filter(model == 'logit_length') |>
    dplyr::select(
      model,
      age,
      `Spring survey` = SigmaSurvey1,
      `Autumn survey` = SigmaSurvey2,
      ` Catch` = SigmaC
    ) |>
    tidyr::gather(survey, sigma, -c(model, age)) |>
    dplyr::filter(sigma > 0, sigma < 0.5) |>
    dplyr::left_join(
      fit$params |>
        dplyr::filter(
          name %in% c('logSigmaCmultiplier', 'SigmaSurveypar'),
          model == 'logit_length'
        ) |>
        dplyr::mutate(
          value = ifelse(name == 'SigmaSurveypar', exp(value), value),
          survey = ifelse(
            name == 'SigmaSurveypar',
            ifelse(index == max(index), 'Spring survey', 'Autumn survey'),
            ' Catch'
          )
        )
    ) |>
    ggplot2::ggplot(ggplot2::aes(age, sigma / value, col = survey)) +
    ggplot2::geom_line() +
    ggplot2::labs(y = bquote(sigma[age]), x = 'Age', col = '') +
    ggplot2::scale_color_manual(values = c('red', 'blue', 'green')) +
    ggplot2::scale_x_continuous(breaks = 2 * (1:10), minor_breaks = 1:20) +
    ggplot2::expand_limits(y = 0)

  selection_plot_3 <-
    fit$rby |>
    dplyr::filter(year < assessment_year, model == 'logit_length') |>
    ggplot2::ggplot(ggplot2::aes(Spawningstock, Recruitment / 1000)) +
    #ggplot2::geom_point(ggplot2::aes(ssbbreak, Rmax/1000),
    #           data = fit$mcmc_results |>
    #             dplyr::filter(variable %in% c('ssbbreak','Rmax')) |>
    #             dplyr::select(iter,variable,value) |>
    #             spread(variable,value),col='lightblue') +
    #ggplot2::geom_point(ggplot2::aes(ssbbreak, Rmax/1000),
    #           data = fit$params |>
    #             dplyr::filter(variable %in% c('ssbbreak','Rmax'),
    #                    model == 'logit_length') |>
    #             dplyr::select(model,variable,value) |>
    #             spread(variable,value),
    #           col = 'red') +
    ggplot2::geom_text(ggplot2::aes(label = year)) +
    ggplot2::expand_limits(x = 0, y = 0) +
    ggplot2::labs(x = 'Spawning stock', y = 'Recruitment')
  selection_plot_4 <-
    fit$rbage |>
    dplyr::filter(model == 'logit_length') |>
    dplyr::select(
      model,
      age,
      `Spring survey` = SurveylnQ1,
      `Autumn survey` = SurveylnQ2
    ) |>
    tidyr::gather(survey, sigma, -c(model, age)) |>
    dplyr::filter(sigma < 0.75) |>
    dplyr::group_by(survey) |>
    #mutate(sigma = exp(sigma), sigma = sigma/max(sigma)) |>
    ggplot2::ggplot(ggplot2::aes(age, sigma, col = survey)) +
    ggplot2::geom_line() +
    ggplot2::labs(y = bquote(q[survey]), x = 'Age', col = '') +
    ggplot2::scale_color_manual(values = c('blue', 'green')) +
    ggplot2::theme(
      legend.background = ggplot2::element_blank(),
      legend.position = 'none'
    ) +
    ggplot2::scale_x_continuous(breaks = 2 * (1:5), minor_breaks = 1:10)

  patchwork::wrap_plots(
    selection_plot_1,
    selection_plot_2,
    selection_plot_3,
    selection_plot_4,
    ncol = 2
  )
}
