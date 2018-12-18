# TODO: Auf Module umstellen
# Weniger wichtig: Multinomialverteilung

#' @export
show_specific_distribution_input <- function(prefix_id, distribution) {
  divId <- paste(prefix_id, distribution, "div", sep = "_")
  shinyjs::show(
    selector = paste("#", divId, sep = "")
  )
}

#' @export
hide_all_specific_distribution_inputs <- function(prefix_id) {
  divClass <- paste(prefix_id, "class", sep = "_")
  shinyjs::hide(
    selector = paste(".", divClass, sep = "")
  )
}

# SPECIFIC DISTRIBUTION INPUT --------------------------------------------------
# Jede Verteilung besitzt spezifische Verteilungsparameter. prefix_id stellt den Namensraum
# für die inputIds zur Verfügung
# ÜBERLEGUNG: Das ganze als eigenes Modul umsetzen

#' @export
get_specific_distribution_input <- function(session, input, .values, distribution, index) {
  ui_element <- switch(
    EXPR = distribution,
    "binom" = get_specific_binom_input(session, input, .values, index),
    "geom" = get_specific_geom_input(session, input, .values, index),
    "hyper" = get_specific_hyper_input(session, input, .values, index),
    "multinom" = get_specific_multinom_input(session, input, .values, index),
    "nbinom" = get_specific_nbinom_input(session, input, .values, index),
    "pois" = get_specific_pois_input(session, input, .values, index),
    "beta" = get_specific_beta_input(session, input, .values, index),
    "cauchy" = get_specific_cauchy_input(session, input, .values, index),
    "chisq" = get_specific_chisq_input(session, input, .values, index),
    "exp" = get_specific_exp_input(session, input, .values, index),
    "f" = get_specific_f_input(session, input, .values, index),
    "gamma" = get_specific_gamma_input(session, input, .values, index),
    "unif" = get_specific_unif_input(session, input, .values, index),
    "lnorm" = get_specific_lnorm_input(session, input, .values, index),
    "norm" = get_specific_norm_input(session, input, .values, index),
    "t" = get_specific_t_input(session, input, .values, index),
    "weibull" = get_specific_weibull_input(session, input, .values, index)
  )
  return(ui_element)
}

get_specific_binom_input <- function(session, input, .values, index) {
  ns <- session$ns
  size_input_id <- "binom_size" %_% index
  prob_input_id <- "binom_prob" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(size_input_id),
        label = "n",
        value = fallback(input[["binom_size" %_% index]], 10),
        min = 1
      )
    ),
    column(
      width = 6,
      sliderInput(
        inputId = ns(prob_input_id),
        label = "p",
        min = 0,
        max = 1,
        value = fallback(input[["binom_prob" %_% index]], 0.1),
        step = 0.01
      )
    )
  )
  return(ui_element)
}

get_specific_geom_input <- function(session, input, .values, index) {
  ns <- session$ns
  prob_id <- "geom_prob" %_% index
  xmax_id <- "geom_xmax" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      sliderInput(
        inputId = ns(prob_id),
        label = "p",
        min = 0,
        max = 1,
        value = fallback(input[["geom_prob" %_% index]], 0.1),
        step = 0.01
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(xmax_id),
        label = "Xmax",
        value = fallback(input[["geom_xmax" %_% index]], qgeom(p = 0.95, prob = 0.1)),
        min = 1,
        step = 1
      )
    )
  )
  observeEvent(input[[prob_id]], {
    updateNumericInput(
      session = session,
      inputId = xmax_id,
      value = qgeom(p = req(.values$einstellungen$dqe$quantil_xmax), prob = req(input[[prob_id]]))
    )
  })
  observeEvent(.values$einstellungen$dqe$quantil_xmax, {
    updateNumericInput(
      session = session,
      inputId = xmax_id,
      value = qgeom(p = req(.values$einstellungen$dqe$quantil_xmax), prob = req(input[[prob_id]]))
    )
  })
  return(ui_element)
}

get_specific_hyper_input <- function(session, input, .values, index) {
  ns <- session$ns
  m_id <- "hyper_m" %_% index
  n_id <- "hyper_n" %_% index
  k_id <- "hyper_k" %_% index
  ui_element <- fluidRow(
    column(
      width = 4,
      numericInput(
        inputId = ns(m_id),
        label = label_lang(
          de = "Weisse Bälle",
          en = "White Balls"
        ),
        value = fallback(input[["hyper_m" %_% index]], 5),
        min = 0
      )
    ),
    column(
      width = 4,
      numericInput(
        inputId = ns(n_id),
        label = label_lang(
          de = "Schwarze Bälle",
          en = "Black balls"
        ),
        value = fallback(input[["hyper_n" %_% index]], 5),
        min = 0
      )
    ),
    column(
      width = 4,
      numericInput(
        inputId = ns(k_id),
        label = label_lang(
          de = "Gezogene Bälle",
          en = "Drawn balls"
        ),
        value = fallback(input[["hyper_k" %_% index]], 5),
        min = 0
      )
    )
  )
  return(ui_element)
}

get_specific_multinom_input <- function(session, input, .values, index) {
  ns <- session$ns
  ui_element <- div(
    "TODO: Multinomialverteilung, PROBLEM: 1. Argument x, weiterhin keine Verteilungs- und dichtefunktion"
  )
  return(ui_element)
}

get_specific_nbinom_input <- function(session, input, .values, index) {
  ns <- session$ns
  size_id <- "nbinom_size" %_% index
  prob_id <- "nbinom_prob" %_% index
  xmax_id <- "nbinom_xmax" %_% index
  ui_element <- fluidRow(
    column(
      width = 4,
      numericInput(
        inputId = ns(size_id),
        label = "n",
        value = fallback(input[["nbinom_size" %_% index]], 10),
        min = 0
      )
    ),
    column(
      width = 4,
      sliderInput(
        inputId = ns(prob_id),
        label = "p",
        value = fallback(input[["nbinom_prob" %_% index]], 0.1),
        min = 0,
        max = 1,
        step = 0.01
      )
    ),
    column(
      width = 4,
      numericInput(
        inputId = ns(xmax_id),
        label = "Xmax",
        value = fallback(
          input[["nbinom_xmax" %_% index]],
          qnbinom(p = 0.95, size = 10, prob = 0.1)
        ),
        min = 1,
        step = 1
      )
    )
  )
  observeEvent(input[[size_id]], {
    updateNumericInput(
      session = session,
      inputId = xmax_id,
      value = qnbinom(p = req(.values$einstellungen$dqe$quantil_xmax), size = req(input[[size_id]]), prob = req(input[[prob_id]]))
    )
  })
  observeEvent(input[[prob_id]], {
    updateNumericInput(
      session = session,
      inputId = xmax_id,
      value = qnbinom(p = req(.values$einstellungen$dqe$quantil_xmax), size = req(input[[size_id]]), prob = req(input[[prob_id]]))
    )
  })
  observeEvent(.values$einstellungen$dqe$quantil_xmax, {
    updateNumericInput(
      session = session,
      inputId = xmax_id,
      value = qnbinom(p = req(.values$einstellungen$dqe$quantil_xmax), size = req(input[[size_id]]), prob = req(input[[prob_id]]))
    )
  })
  return(ui_element)
}

get_specific_pois_input <- function(session, input, .values, index) {
  ns <- session$ns
  lambda_id <- "pois_lambda" %_% index
  xmax_id <- "pois_xmax" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(lambda_id),
        label = "Lambda",
        value = fallback(input[["pois_lambda" %_% index]], 1),
        min = 0,
        step = 0.01
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(xmax_id),
        label = "Xmax",
        value = fallback(
          input[["pois_xmax" %_% index]],
          qpois(p = 0.95, lambda = 1)
        ),
        min = 0,
        step = 1
      )
    )
  )
  observeEvent(input[[lambda_id]], {
    updateNumericInput(
      session = session,
      inputId = xmax_id,
      value = qpois(p = req(.values$einstellungen$dqe$quantil_xmax), lambda = req(input[[lambda_id]]))
    )
  })
  observeEvent(.values$einstellungen$dqe$quantil_xmax, {
    updateNumericInput(
      session = session,
      inputId = xmax_id,
      value = qpois(p = req(.values$einstellungen$dqe$quantil_xmax), lambda = req(input[[lambda_id]]))
    )
  })
  return(ui_element)
}

get_specific_beta_input <- function(session, input, .values, index) {
  ns <- session$ns
  shape1_id <- "beta_shape1" %_% index
  shape2_id <- "beta_shape2" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(shape1_id),
        label = label_lang(
          de = "Form 1",
          en = "Shape 1"
        ),
        value = fallback(input[["beta_shape1" %_% index]], 1),
        min = 0,
        step = 0.01
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(shape2_id),
        label = label_lang(
          de = "Form 2",
          en = "Shape 2"
        ),
        value = fallback(input[["beta_shape2" %_% index]], 1),
        min = 0,
        step = 0.01
      )
    )
  )
  return(ui_element)
}

get_specific_cauchy_input <- function(session, input, .values, index) {
  ns <- session$ns
  location_id <- "cauchy_location" %_% index
  scale_id <- "cauchy_scale" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(location_id),
        label = label_lang(
          de = "Form",
          en = "Location"
        ),
        value = fallback(input[["cauchy_location" %_% index]], 0)
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(scale_id),
        label = label_lang(
          de = "Skalierung",
          en = "Scale"
        ),
        value = fallback(input[["cauchy_scale" %_% index]], 1)
      )
    )
  )
  return(ui_element)
}

get_specific_chisq_input <- function(session, input, .values, index) {
  ns <- session$ns
  df_id <- "chisq_df" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(df_id),
        label = label_lang(
          de = "Freiheitsgrade",
          en = "Degrees of freedom"
        ),
        value = fallback(input[["chisq_df" %_% index]], 1),
        min = 1
      )
    )
  )
  return(ui_element)
}

get_specific_exp_input <- function(session, input, .values, index) {
  ns <- session$ns
  rate_id <- "exp_rate" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(rate_id),
        label = "Rate",
        value = fallback(input[["exp_rate" %_% index]], 1),
        min = 0,
        step = 0.01
      )
    )
  )
  return(ui_element)
}

get_specific_f_input <- function(session, input, .values, index) {
  ns <- session$ns
  df1_id <- "f_df1" %_% index
  df2_id <- "f_df2" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(df1_id),
        label = "df1",
        value = fallback(input[["f_df1" %_% index]], 1),
        min = 0
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(df2_id),
        label = "df2",
        value = fallback(input[["f_df2" %_% index]], 1),
        min = 0
      )
    )
  )
  return(ui_element)
}

get_specific_gamma_input <- function(session, input, .values, index) {
  ns <- session$ns
  shape_id <- "gamma_shape" %_% index
  scale_id <- "gamma_scale" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(shape_id),
        label = "Form",
        value = fallback(input[["gamma_shape" %_% index]], 1),
        min = 0,
        step = 0.01
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(scale_id),
        label = label_lang(
          de = "Skalierung",
          en = "Scale"
        ),
        value = fallback(input[["gamma_scale" %_% index]], 1),
        min = 0.01,
        step = 0.01
      )
    )
  )
  return(ui_element)
}

get_specific_unif_input <- function(session, input, .values, index) {
  ns <- session$ns
  min_id <- "unif_min" %_% index
  max_id <- "unif_max" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(min_id),
        "Min",
        value = fallback(input[["unif_min" %_% index]], 0)
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(max_id),
        "Max",
        value = fallback(input[["unif_max" %_% index]], 1)
      )
    )
  )
  return(ui_element)
}

get_specific_lnorm_input <- function(session, input, .values, index) {
  ns <- session$ns
  meanlog_id <- "lnorm_meanlog" %_% index
  sdlog_id <- "lnorm_sdlog" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(meanlog_id),
        label = "mu",
        value = fallback(input[["lnorm_meanlog" %_% index]], 0)
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(sdlog_id),
        label = "sigma",
        value = fallback(input[["lnorm_sdlog" %_% index]], 1),
        min = 0.01
      )
    )
  )
  return(ui_element)
}

get_specific_norm_input <- function(session, input, .values, index) {
  ns <- session$ns
  mean_id <- "norm_mean" %_% index
  sd_id <- "norm_sd" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(mean_id),
        label = "mu",
        value = fallback(input[["norm_mean" %_% index]], 0)
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(sd_id),
        label = "sigma",
        value = fallback(input[["norm_sd" %_% index]], 1),
        min = 0.01
      )
    )
  )
  return(ui_element)
}

get_specific_t_input <- function(session, input, .values, index) {
  ns <- session$ns
  df_id <- "t_df" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(df_id),
        label = label_lang(
          de = "Freiheitsgrade",
          en = "Degrees of freedom"
        ),
        value = fallback(input[["t_df" %_% index]], 1),
        min = 0
      )
    )
  )
  return(ui_element)
}

get_specific_weibull_input <- function(session, input, .values, index) {
  ns <- session$ns
  shape_id <- "weibull_shape" %_% index
  scale_id <- "weibull_scale" %_% index
  ui_element <- fluidRow(
    column(
      width = 6,
      numericInput(
        inputId = ns(shape_id),
        label = "Form",
        value = fallback(input[["weibull_shape" %_% index]], 1)
      )
    ),
    column(
      width = 6,
      numericInput(
        inputId = ns(scale_id),
        label = label_lang(
          de = "Skalierung",
          en = "Scale"
        ),
        value = fallback(input[["weibull_scale" %_% index]], 1)
      )
    )
  )
  return(ui_element)
}

# SPECIFIC DISTRIBUTION PLOTS --------------------------------------------------

#' @export
get_distribution_plot <- function(input, .values, prefix_id, distribution, type, plot_engine) {
  args <- switch(
    EXPR = distribution,
    "binom" = get_binom_plot(input, .values, prefix_id, type, plot_engine),
    "geom" = get_geom_plot(input, .values, prefix_id, type, plot_engine),
    "hyper" = get_hyper_plot(input, .values, prefix_id, type, plot_engine),
    "multinom" = get_multinom_plot(input, .values, prefix_id, type, plot_engine),
    "nbinom" = get_nbinom_plot(input, .values, prefix_id, type, plot_engine),
    "pois" = get_pois_plot(input, .values, prefix_id, type, plot_engine),
    "beta" = get_beta_plot(input, .values, prefix_id, type, plot_engine),
    "cauchy" = get_cauchy_plot(input, .values, prefix_id, type, plot_engine),
    "chisq" = get_chisq_plot(input, .values, prefix_id, type, plot_engine),
    "exp" = get_exp_plot(input, .values, prefix_id, type, plot_engine),
    "f" = get_f_plot(input, .values, prefix_id, type, plot_engine),
    "gamma" = get_gamma_plot(input, .values, prefix_id, type, plot_engine),
    "unif" = get_unif_plot(input, .values, prefix_id, type, plot_engine),
    "lnorm" = get_lnorm_plot(input, .values, prefix_id, type, plot_engine),
    "norm" = get_norm_plot(input, .values, prefix_id, type, plot_engine),
    "t" = get_t_plot(input, .values, prefix_id, type, plot_engine),
    "weibull" = get_weibull_plot(input, .values, prefix_id, type, plot_engine)
  )
  return(args)
}

get_binom_plot <- function(input, .values, prefix_id, type, plot_engine) {
  sizeId <- paste(prefix_id, "binom_size", sep = "_")
  probId <- paste(prefix_id, "binom_prob", sep = "_")
  size <- input[[sizeId]]
  prob <- input[[probId]]
  req(size, prob)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, size, by = 1), y = dbinom(x = x, size = size, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = .values$einstellungen$ggplot2$col, fill = .values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = 0:size, y = pbinom(q = x, size = size, prob = prob))
      print(.values$einstellungen$ggplot2$col)
      print(.values$einstellungen$ggplot2$size)
      plot <- ggplot(data = data) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        scale_x_continuous(breaks = 0:size) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "x", y = expression(paste(F(x) == P(X <= x)))) +
        theme_bw()
    } else if (type == "quantile") {
      x <- seq(0, size, by = 1)
      px <- pbinom(q = x, size = size, prob = prob)
      left_x <- px[1:(length(px) - 1)]
      right_x <- px[2:(length(px))]
      left_y <- x[1:(length(x) - 1)]
      right_y <- x[2:(length(x))]
      plot <- ggplot() +
        geom_segment(mapping = aes(x = left_x, xend = right_x, y = left_y, yend = right_y)) +
        theme_bw()
    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_geom_plot <- function(input, .values, prefix_id, type, plot_engine) {
  probId <- paste(prefix_id, "geom_prob", sep = "_")
  xmaxId <- paste(prefix_id, "geom_xmax", sep = "_")
  prob <- input[[probId]]
  xmax <- input[[xmaxId]]
  req(prob, xmax)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, xmax), y = dgeom(x = x, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = .values$einstellungen$ggplot2$col, fill = .values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, xmax), y = pgeom(q = x, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        scale_x_continuous(breaks = 0:xmax) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "x", y = expression(paste(F(x) == P(X <= x)))) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_hyper_plot <- function(input, .values, prefix_id, type, plot_engine) {
  m <- input[[paste(prefix_id, "hyper_m", sep = "_")]]
  n <- input[[paste(prefix_id, "hyper_n", sep = "_")]]
  k <- input[[paste(prefix_id, "hyper_k", sep = "_")]]
  req(m, n, k)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, k), y = dhyper(x = x, m = m, n = n, k = k))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = .values$einstellungen$ggplot2$col, fill = .values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, k), y = phyper(q = x, m = m, n = n, k = k))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        scale_x_continuous(breaks = 0:k) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "x", y = expression(paste(F(x) == P(X <= x)))) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_multinom_plot <- function(input, .values, prefix_id, type, plot_engine) {
  args_list <- list(
    NULL
  )
  return(args_list)
}

get_nbinom_plot <- function(input, .values, prefix_id, type, plot_engine) {
  size <- input[[paste(prefix_id, "nbinom_size", sep = "_")]]
  prob <- input[[paste(prefix_id, "nbinom_prob", sep = "_")]]
  xmax <- input[[paste(prefix_id, "nbinom_xmax", sep = "_")]]
  req(size, prob, xmax)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, size), y = dnbinom(x = x, size = size, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = .values$einstellungen$ggplot2$col, fill = .values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, xmax), y = pnbinom(q = x, size = size, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        scale_x_continuous(breaks = 0:xmax) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "x", y = expression(paste(F(x) == P(X <= x)))) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_pois_plot <- function(input, .values, prefix_id, type, plot_engine) {
  lambda <- input[[paste(prefix_id, "pois_lambda", sep = "_")]]
  xmax <- input[[paste(prefix_id, "pois_xmax", sep = "_")]]
  req(lambda, xmax)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, xmax), y = dpois(x = x, lambda = lambda))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = .values$einstellungen$ggplot2$col, fill = .values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, xmax), y = ppois(q = x, lambda = lambda))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = .values$einstellungen$ggplot2$col,
                   size = 2.5 * .values$einstellungen$ggplot2$size) +
        scale_x_continuous(breaks = 0:xmax) +
        scale_y_continuous(limits = c(0, 1)) +
        labs(x = "x", y = expression(paste(F(x) == P(X <= x)))) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_beta_plot <- function(input, .values, prefix_id, type, plot_engine) {
  shape1 <- input[[paste(prefix_id, "beta_shape1", sep = "_")]]
  shape2 <- input[[paste(prefix_id, "beta_shape2", sep = "_")]]
  req(shape1, shape2)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, 1, length.out = 500), y = dbeta(x = x, shape1, shape2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, 1, length.out = 500), y = pbeta(q = x, shape1, shape2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_cauchy_plot <- function(input, .values, prefix_id, type, plot_engine) {
  location <- input[[paste(prefix_id, "cauchy_location", sep = "_")]]
  scale <- input[[paste(prefix_id, "cauchy_scale", sep = "_")]]
  req(location, scale)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qcauchy(0.01, location, scale), qcauchy(0.99, location, scale), length.out = 500),
                     y = dcauchy(x = x, location, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qcauchy(0.01, location, scale), qcauchy(0.99, location, scale), length.out = 500),
                     y = pcauchy(q = x, location, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_chisq_plot <- function(input, .values, prefix_id, type, plot_engine) {
  df <- input[[paste(prefix_id, "chisq_df", sep = "_")]]
  req(df)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qchisq(0.99, df), length.out = 500), y = dchisq(x = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qchisq(0, df), qchisq(0.99, df), length.out = 500), y = pchisq(q = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_exp_plot <- function(input, .values, prefix_id, type, plot_engine) {
  rate <- input[[paste(prefix_id, "exp_rate", sep = "_")]]
  req(rate)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qexp(0.99, rate), length.out = 500), y = dexp(x = x, rate))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, qexp(0.99, rate), length.out = 500), y = pexp(q = x, rate))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_f_plot <- function(input, .values, prefix_id, type, plot_engine) {
  df1 <- input[[paste(prefix_id, "f_df1", sep = "_")]]
  df2 <- input[[paste(prefix_id, "f_df2", sep = "_")]]
  req(df1, df2)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qf(0.99, df1, df2), length.out = 500), y = df(x = x, df1, df2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, qf(0.99, df1, df2), length.out = 500), y = pf(q = x, df1, df2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_gamma_plot <- function(input, .values, prefix_id, type, plot_engine) {
  shape <- input[[paste(prefix_id, "gamma_shape", sep = "_")]]
  scale <- input[[paste(prefix_id, "gamma_scale", sep = "_")]]
  req(shape, scale)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qgamma(0.01, shape = shape, scale = scale), qgamma(0.99, shape = shape, scale = scale), length.out = 500),
                     y = dgamma(x = x, shape = shape, scale = scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qgamma(0.01, shape = shape, scale = scale), qgamma(0.99, shape = shape, scale = scale), length.out = 500),
                     y = pgamma(q = x, shape = shape, scale = scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_lnorm_plot <- function(input, .values, prefix_id, type, plot_engine) {
  meanlog <- input[[paste(prefix_id, "lnorm_meanlog", sep = "_")]]
  sdlog <- input[[paste(prefix_id, "lnorm_sdlog", sep = "_")]]
  req(meanlog, sdlog)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qlnorm(0.99, meanlog, sdlog), length.out = 500),
                     y = dlnorm(x = x, meanlog, sdlog))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, qlnorm(0.99, meanlog, sdlog), length.out = 500),
                     y = plnorm(q = x, meanlog, sdlog))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_norm_plot <- function(input, .values, prefix_id, type, plot_engine) {
  mean <- input[[paste(prefix_id, "norm_mean", sep = "_")]]
  sd <- input[[paste(prefix_id, "norm_sd", sep = "_")]]
  req(mean, sd)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qnorm(0.01, mean, sd), qnorm(0.99, mean, sd), length.out = 500),
                     y = dnorm(x = x, mean, sd))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qnorm(0.01, mean, sd), qnorm(0.99, mean, sd), length.out = 500),
                     y = pnorm(q = x, mean, sd))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_t_plot <- function(input, .values, prefix_id, type, plot_engine) {
  df <- input[[paste(prefix_id, "t_df", sep = "_")]]
  req(df)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qt(0.01, df), qt(0.99, df), length.out = 500),
                     y = dt(x = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qt(0.01, df), qt(0.99, df), length.out = 500),
                     y = pt(q = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_unif_plot <- function(input, .values, prefix_id, type, plot_engine) {
  min <- input[[paste(prefix_id, "unif_min", sep = "_")]]
  max <- input[[paste(prefix_id, "unif_max", sep = "_")]]
  req(min, max)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(min - 1, max + 1, length.out = 500),
                     y = dunif(x = x, min, max))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(min - 1, max + 1, length.out = 500),
                     y = punif(q = x, min, max))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

get_weibull_plot <- function(input, .values, prefix_id, type, plot_engine) {
  shape <- input[[paste(prefix_id, "weibull_shape", sep = "_")]]
  scale <- input[[paste(prefix_id, "weibull_scale", sep = "_")]]
  req(shape, scale)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qweibull(0.01, shape, scale), qweibull(0.99, shape, scale), length.out = 500),
                     y = dweibull(x = x, shape, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- dplyr::tibble(x = seq(qweibull(0.01, shape, scale), qweibull(0.99, shape, scale), length.out = 500),
                     y = pweibull(q = x, shape, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = .values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "quantile") {

    }
  } else if (plot_engine == "plotly") {
    if (type == "density") {

    } else if (type == "probablity") {

    } else if (type == "quantile") {

    }
  }
  return(plot)
}

# GET ARG VALUES --------------------------------

get_arg_values <- function(session, distribution, index) {
  # arg_values <- switch(
  #   EXPR = distribution,
  #   "binom" = get_binom_arg_values(session, index),
  #   "geom" = get_geom_arg_values(session, index),
  #   "hyper" = get_hyper_arg_values(session, index),
  #   "multinom" = get_multinom_arg_values(session, index),
  #   "nbinom" = get_nbinom_arg_values(session, index),
  #   "pois" = get_pois_arg_values(session, index),
  #   "beta" = get_beta_arg_values(session, index),
  #   "cauchy" = get_cauchy_arg_values(session, index),
  #   "chisq" = get_chisq_arg_values(session, index),
  #   "exp" = get_exp_arg_values(session, index),
  #   "f" = get_f_arg_values(session, index),
  #   "gamma" = get_gamma_arg_values(session, index),
  #   "unif" = get_unif_arg_values(session, index),
  #   "lnorm" = get_lnorm_arg_values(session, index),
  #   "norm" = get_norm_arg_values(session, index),
  #   "t" = get_t_arg_values(session, index),
  #   "weibull" = get_weibull_arg_values(session, index)
  # )
  arg_names <- switch(
    distribution,
    "binom" = c("size", "prob"),
    "geom" = c("prob", "xmax"),
    "hyper" = c("m", "n", "k"),
    "nbinom" = c("size", "prob", "xmax"),
    "pois" = c("lambda", "xmax"),
    "beta" = c("shape1", "shape2"),
    "cauchy" = c("location", "scale"),
    "chisq" = c("df"),
    "exp" = c("rate"),
    "f" = c("df1", "df2"),
    "gamma" = c("shape", "scale"),
    "unif" = c("min", "max"),
    "lnorm" = c("meanlog", "sdlog"),
    "norm" = c("mean", "sd"),
    "t" = c("df"),
    "weibull" = c("shape", "scale")
  )
  discrete <- ifelse(distribution %in% c("binom", "geom", "hyper", "nbinom", "pois"), TRUE, FALSE)
  arg_values <- get_arg_values_data_table(
    session = session,
    index = index,
    distribution = distribution,
    name = arg_names,
    discrete = discrete
  )
  return(arg_values)
}

get_arg_values_data_table <- function(session, index, distribution, name, discrete) {
  input <- session$input
  arg_values <- data.table(
    index = index,
    distribution = distribution,
    name = name,
    value = map_dbl(name, function(name) {
      req(input[[distribution %_% name %_% index]])
    }),
    discrete = discrete
  )
}

get_binom_arg_values <- function(session, index) {
  arg_values <- list(
    size = input[["binom_size" %_% index]],
    prob = input[["binom_prob" %_% index]]
  )
  arg_values <- data.table(
    index = index,
    distribution = "binom",
    name = c("size", "prob"),
    value = c(input[["binom_size" %_% index]],
              input[["binom_prob" %_% index]])
  )
}

get_geom_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    prob = input[["geom_prob" %_% index]],
    xmax = input[["geom_xmax" %_% index]]
  )
}

get_hyper_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    m = input[["hyper_m" %_% index]],
    n = input[["hyper_n" %_% index]],
    k = input[["hyper_k" %_% index]]
  )
}

get_multinom_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
  )
}

get_nbinom_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    size = input[["nbinom_size" %_% index]],
    prob = input[["nbinom_prob" %_% index]],
    xmax = input[["nbinom_xmax" %_% index]]
  ) 
}

get_pois_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    lambda = input[["pois_lambda" %_% index]],
    xmax = input[["pois_xmax" %_% index]]
  )
}

get_beta_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    shape1 = input[["beta_shape1" %_% index]],
    shape2 = input[["beta_shape2" %_% index]]
  )
}

get_cauchy_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    location = input[["cauchy_location" %_% index]],
    scale = input[["cauchy_scale" %_% index]]
  )
}

get_chisq_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    df = input[["chisq_df" %_% index]]
  )
}

get_exp_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    rate = input[["exp_rate" %_% index]]
  )
}

get_f_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    df1 = input[["f_df2" %_% index]],
    df2 = input[["f_df1" %_% index]]
  )
}

get_gamma_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    shape = input[["gamma_shape" %_% index]],
    scale = input[["gamma_scale" %_% index]]
  )
}

get_unif_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    min = input[["unif_min" %_% index]],
    max = input[["unif_max" %_% index]]
  )
}

get_lnorm_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    meanlog = input[["lnorm_meanlog" %_% index]],
    sdlog = input[["lnorm_sdlog" %_% index]]
  )
}

get_norm_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    mean = input[["norm_mean" %_% index]],
    sd = input[["norm_sd" %_% index]]
  )
}

get_t_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    df = input[["t_df" %_% index]]
  )
}

get_weibull_arg_values <- function(session, index) {
  input <- session$input
  arg_values <- list(
    shape = input[["weibull_shape" %_% index]],
    scale = input[["weibull_scale" %_% index]]
  )
}



