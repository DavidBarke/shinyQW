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

# SPECIFIC DISTRIBUTION INPUT --------------------------------------------------------------
# Jede Verteilung besitzt spezifische Verteilungsparameter. prefix_id stellt den Namensraum
# für die inputIds zur Verfügung
# ÜBERLEGUNG: Das ganze als eigenes Modul umsetzen

#' @export
get_specific_distribution_input <- function(session, input, values, prefix_id, distribution) {
  ui_element <- switch(
    EXPR = distribution,
    "binom" = get_specific_binom_input(session, input, values, prefix_id),
    "geom" = get_specific_geom_input(session, input, values, prefix_id),
    "hyper" = get_specific_hyper_input(session, input, values, prefix_id),
    "multinom" = get_specific_multinom_input(session, input, values, prefix_id),
    "nbinom" = get_specific_nbinom_input(session, input, values, prefix_id),
    "pois" = get_specific_pois_input(session, input, values, prefix_id),
    "beta" = get_specific_beta_input(session, input, values, prefix_id),
    "cauchy" = get_specific_cauchy_input(session, input, values, prefix_id),
    "chisq" = get_specific_chisq_input(session, input, values, prefix_id),
    "exp" = get_specific_exp_input(session, input, values, prefix_id),
    "f" = get_specific_f_input(session, input, values, prefix_id),
    "gamma" = get_specific_gamma_input(session, input, values, prefix_id),
    "unif" = get_specific_unif_input(session, input, values, prefix_id),
    "lnorm" = get_specific_lnorm_input(session, input, values, prefix_id),
    "norm" = get_specific_norm_input(session, input, values, prefix_id),
    "t" = get_specific_t_input(session, input, values, prefix_id),
    "weibull" = get_specific_weibull_input(session, input, values, prefix_id)
  )
  return(ui_element)
}

get_specific_binom_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "binom_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  sizeInputId <- paste(prefix_id, "binom_size", sep = "_")
  probInputId <- paste(prefix_id, "binom_prob", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(sizeInputId),
          label = "n",
          value = 10,
          min = 1
        )
      ),
      column(
        width = 6,
        sliderInput(
          inputId = ns(probInputId),
          label = "p",
          min = 0,
          max = 1,
          value = 0.1,
          step = 0.01
        )
      )
    )
  )
  return(ui_element)
}

get_specific_geom_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "geom_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  probId <- paste(prefix_id, "geom_prob", sep = "_")
  xmaxId <- paste(prefix_id, "geom_xmax", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        sliderInput(
          inputId = ns(probId),
          label = "p",
          min = 0,
          max = 1,
          value = 0.1,
          step = 0.01
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(xmaxId),
          label = "Xmax",
          value = qgeom(p = 0.95, prob = 0.1),
          min = 1,
          step = 1
        )
      )
    )
  )
  observeEvent(input[[probId]], {
    updateNumericInput(
      session = session,
      inputId = xmaxId,
      value = qgeom(p = req(values$einstellungen$dqe$quantil_xmax), prob = req(input[[probId]]))
    )
  })
  observeEvent(values$einstellungen$dqe$quantil_xmax, {
    updateNumericInput(
      session = session,
      inputId = xmaxId,
      value = qgeom(p = req(values$einstellungen$dqe$quantil_xmax), prob = req(input[[probId]]))
    )
  })
  return(ui_element)
}

get_specific_hyper_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "hyper_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  mId <- paste(prefix_id, "hyper_m", sep = "_")
  nId <- paste(prefix_id, "hyper_n", sep = "_")
  kId <- paste(prefix_id, "hyper_k", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 4,
        numericInput(
          inputId = ns(mId),
          label = "Weisse Bälle",
          value = 5,
          min = 0
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = ns(nId),
          label = "Schwarze Bälle",
          value = 5,
          min = 0
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = ns(kId),
          label = "Gezogene Bälle",
          value = 5,
          min = 0
        )
      )
    )
  )
  return(ui_element)
}

get_specific_multinom_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "multinom_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    "TODO: Multinomialverteilung, PROBLEM: 1. Argument x, weiterhin keine Verteilungs- und dichtefunktion"
  )
  return(ui_element)
}

get_specific_nbinom_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "nbinom_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  sizeId <- paste(prefix_id, "nbinom_size", sep = "_")
  probId <- paste(prefix_id, "nbinom_prob", sep = "_")
  xmaxId <- paste(prefix_id, "nbinom_xmax", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(sizeId),
          label = "n",
          value = 10,
          min = 0
        )
      ),
      column(
        width = 6,
        sliderInput(
          inputId = ns(probId),
          label = "p",
          value = 0.1,
          min = 0,
          max = 1,
          step = 0.01
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(xmaxId),
          label = "Xmax",
          value = qnbinom(p = 0.95, size = 10, prob = 0.1),
          min = 1,
          step = 1
        )
      )
    )
  )
  observeEvent(input[[sizeId]], {
    updateNumericInput(
      session = session,
      inputId = xmaxId,
      value = qnbinom(p = req(values$einstellungen$dqe$quantil_xmax), size = req(input[[sizeId]]), prob = req(input[[probId]]))
    )
  })
  observeEvent(input[[probId]], {
    updateNumericInput(
      session = session,
      inputId = xmaxId,
      value = qnbinom(p = req(values$einstellungen$dqe$quantil_xmax), size = req(input[[sizeId]]), prob = req(input[[probId]]))
    )
  })
  observeEvent(values$einstellungen$dqe$quantil_xmax, {
    updateNumericInput(
      session = session,
      inputId = xmaxId,
      value = qnbinom(p = req(values$einstellungen$dqe$quantil_xmax), size = req(input[[sizeId]]), prob = req(input[[probId]]))
    )
  })
  return(ui_element)
}

get_specific_pois_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "pois_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  lambdaId <- paste(prefix_id, "pois_lambda", sep = "_")
  xmaxId <- paste(prefix_id, "pois_xmax", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(lambdaId),
          label = "Lambda",
          value = 1,
          min = 0,
          step = 0.01
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(xmaxId),
          label = "Xmax",
          value = qpois(p = 0.95, lambda = 1),
          min = 0,
          step = 1
        )
      )
    )
  )
  observeEvent(input[[lambdaId]], {
    updateNumericInput(
      session = session,
      inputId = xmaxId,
      value = qpois(p = req(values$einstellungen$dqe$quantil_xmax), lambda = req(input[[lambdaId]]))
    )
  })
  observeEvent(values$einstellungen$dqe$quantil_xmax, {
    updateNumericInput(
      session = session,
      inputId = xmaxId,
      value = qpois(p = req(values$einstellungen$dqe$quantil_xmax), lambda = req(input[[lambdaId]]))
    )
  })
  return(ui_element)
}

get_specific_beta_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "beta_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  shape1Id <- paste(prefix_id, "beta_shape1", sep = "_")
  shape2Id <- paste(prefix_id, "beta_shape2", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(shape1Id),
          label = "Shape 1",
          value = 1,
          min = 0,
          step = 0.01
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(shape2Id),
          label = "Shape 2",
          value = 1,
          min = 0,
          step = 0.01
        )
      )
    )
  )
  return(ui_element)
}

get_specific_cauchy_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "cauchy_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  locationId <- paste(prefix_id, "cauchy_location", sep = "_")
  scaleId <- paste(prefix_id, "cauchy_scale", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(locationId),
          label = "Location",
          value = 0
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(scaleId),
          label = "Scale",
          value = 1
        )
      )
    )
  )
  return(ui_element)
}

get_specific_chisq_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "chisq_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  dfId <- paste(prefix_id, "chisq_df", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(dfId),
          label = "Freiheitsgrade",
          value = 1,
          min = 1
        )
      )
    )
  )
  return(ui_element)
}

get_specific_exp_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "exp_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  rateId <- paste(prefix_id, "exp_rate", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(rateId),
          label = "Rate",
          value = 1,
          min = 0,
          step = 0.01
        )
      )
    )
  )
  return(ui_element)
}

get_specific_f_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "f_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  df1Id <- paste(prefix_id, "f_df1", sep = "_")
  df2Id <- paste(prefix_id, "f_df2", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(df1Id),
          label = "df1",
          value = 1,
          min = 0
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(df2Id),
          label = "df2",
          value = 1,
          min = 0
        )
      )
    )
  )
  return(ui_element)
}

get_specific_gamma_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "gamma_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  shapeId <- paste(prefix_id, "gamma_shape", sep = "_")
  scaleId <- paste(prefix_id, "gamma_scale", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(shapeId),
          label = "Form",
          value = 1,
          min = 0,
          step = 0.01
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(scaleId),
          label = "Skalierung",
          value = 1,
          min = 0.01,
          step = 0.01
        )
      )
    )
  )
  return(ui_element)
}

get_specific_unif_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "unif_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  minId <- paste(prefix_id, "unif_min", sep = "_")
  maxId <- paste(prefix_id, "unif_max", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(minId),
          "Min",
          value = 0
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(maxId),
          "Max",
          value = 1
        )
      )
    )
  )
  return(ui_element)
}

get_specific_lnorm_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "lnorm_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  meanlogId <- paste(prefix_id, "lnorm_meanlog", sep = "_")
  sdlogId <- paste(prefix_id, "lnorm_sdlog", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(meanlogId),
          label = "mu",
          value = 0
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(sdlogId),
          label = "sigma",
          value = 1,
          min = 0.01
        )
      )
    )
  )
  return(ui_element)
}

get_specific_norm_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "norm_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  meanId <- paste(prefix_id, "norm_mean", sep = "_")
  sdId <- paste(prefix_id, "norm_sd", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(meanId),
          label = "mu",
          value = 0
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(sdId),
          label = "sigma",
          value = 1,
          min = 0.01
        )
      )
    )
  )
  return(ui_element)
}

get_specific_t_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "t_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  dfId <- paste(prefix_id, "t_df", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(dfId),
          label = "Freiheitsgrade",
          value = 1,
          min = 0
        )
      )
    )
  )
  return(ui_element)
}

get_specific_weibull_input <- function(session, input, values, prefix_id) {
  ns <- session$ns
  divId <- paste(prefix_id, "weibull_div", sep = "_")
  divClass <- paste(prefix_id, "class", sep = "_")
  shapeId <- paste(prefix_id, "weibull_shape", sep = "_")
  scaleId <- paste(prefix_id, "weibull_scale", sep = "_")
  ui_element <- div(
    id = divId,
    class = divClass,
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = ns(shapeId),
          label = "Form",
          value = 1
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns(scaleId),
          label = "Skalierung",
          value = 1
        )
      )
    )
  )
  return(ui_element)
}

# SPECIFIC DISTRIBUTION ARGS --------------------------------------------------------
# args_list enthält Parameter, die die Erstellung der Plots beeinflussen:
# distribution_args: spezifische Verteilungsparameter
# geom_function: Funktion für ggplot2
# geom_function_args: Argumente für geom_function
# x_steps: x-Werte, an denen die Plotfunktion ausgewertet werden soll

#' @export
get_distribution_plot <- function(input, values, prefix_id, distribution, type, plot_engine) {
  args <- switch(
    EXPR = distribution,
    "binom" = get_binom_plot(input, values, prefix_id, type, plot_engine),
    "geom" = get_geom_plot(input, values, prefix_id, type, plot_engine),
    "hyper" = get_hyper_plot(input, values, prefix_id, type, plot_engine),
    "multinom" = get_multinom_plot(input, values, prefix_id, type, plot_engine),
    "nbinom" = get_nbinom_plot(input, values, prefix_id, type, plot_engine),
    "pois" = get_pois_plot(input, values, prefix_id, type, plot_engine),
    "beta" = get_beta_plot(input, values, prefix_id, type, plot_engine),
    "cauchy" = get_cauchy_plot(input, values, prefix_id, type, plot_engine),
    "chisq" = get_chisq_plot(input, values, prefix_id, type, plot_engine),
    "exp" = get_exp_plot(input, values, prefix_id, type, plot_engine),
    "f" = get_f_plot(input, values, prefix_id, type, plot_engine),
    "gamma" = get_gamma_plot(input, values, prefix_id, type, plot_engine),
    "unif" = get_unif_plot(input, values, prefix_id, type, plot_engine),
    "lnorm" = get_lnorm_plot(input, values, prefix_id, type, plot_engine),
    "norm" = get_norm_plot(input, values, prefix_id, type, plot_engine),
    "t" = get_t_plot(input, values, prefix_id, type, plot_engine),
    "weibull" = get_weibull_plot(input, values, prefix_id, type, plot_engine)
  )
  return(args)
}

get_binom_plot <- function(input, values, prefix_id, type, plot_engine) {
  sizeId <- paste(prefix_id, "binom_size", sep = "_")
  probId <- paste(prefix_id, "binom_prob", sep = "_")
  size <- input[[sizeId]]
  prob <- input[[probId]]
  req(size, prob)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, size, by = 1), y = dbinom(x = x, size = size, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = values$einstellungen$ggplot2$col, fill = values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = 0:size, y = pbinom(q = x, size = size, prob = prob))
      print(values$einstellungen$ggplot2$col)
      print(values$einstellungen$ggplot2$size)
      plot <- ggplot(data = data) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
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

get_geom_plot <- function(input, values, prefix_id, type, plot_engine) {
  probId <- paste(prefix_id, "geom_prob", sep = "_")
  xmaxId <- paste(prefix_id, "geom_xmax", sep = "_")
  prob <- input[[probId]]
  xmax <- input[[xmaxId]]
  req(prob, xmax)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, xmax), y = dgeom(x = x, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = values$einstellungen$ggplot2$col, fill = values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, xmax), y = pgeom(q = x, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
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

get_hyper_plot <- function(input, values, prefix_id, type, plot_engine) {
  m <- input[[paste(prefix_id, "hyper_m", sep = "_")]]
  n <- input[[paste(prefix_id, "hyper_n", sep = "_")]]
  k <- input[[paste(prefix_id, "hyper_k", sep = "_")]]
  req(m, n, k)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, k), y = dhyper(x = x, m = m, n = n, k = k))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = values$einstellungen$ggplot2$col, fill = values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, k), y = phyper(q = x, m = m, n = n, k = k))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
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

get_multinom_plot <- function(input, values, prefix_id, type, plot_engine) {
  args_list <- list(
    NULL
  )
  return(args_list)
}

get_nbinom_plot <- function(input, values, prefix_id, type, plot_engine) {
  size <- input[[paste(prefix_id, "nbinom_size", sep = "_")]]
  prob <- input[[paste(prefix_id, "nbinom_prob", sep = "_")]]
  xmax <- input[[paste(prefix_id, "nbinom_xmax", sep = "_")]]
  req(size, prob, xmax)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, size), y = dnbinom(x = x, size = size, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = values$einstellungen$ggplot2$col, fill = values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, xmax), y = pnbinom(q = x, size = size, prob = prob))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
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

get_pois_plot <- function(input, values, prefix_id, type, plot_engine) {
  lambda <- input[[paste(prefix_id, "pois_lambda", sep = "_")]]
  xmax <- input[[paste(prefix_id, "pois_xmax", sep = "_")]]
  req(lambda, xmax)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, xmax), y = dpois(x = x, lambda = lambda))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_col(col = values$einstellungen$ggplot2$col, fill = values$einstellungen$ggplot2$fill) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, xmax), y = ppois(q = x, lambda = lambda))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_segment(mapping = aes(x = x, y = y, xend = x + 1, yend = y)) +
        geom_point(mapping = aes(x = x, y = y), shape = 16,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
        geom_point(mapping = aes(x = x + 1, y = y), shape = 1,
                   col = values$einstellungen$ggplot2$col,
                   size = 2.5 * values$einstellungen$ggplot2$size) +
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

get_beta_plot <- function(input, values, prefix_id, type, plot_engine) {
  shape1 <- input[[paste(prefix_id, "beta_shape1", sep = "_")]]
  shape2 <- input[[paste(prefix_id, "beta_shape2", sep = "_")]]
  req(shape1, shape2)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, 1, length.out = 500), y = dbeta(x = x, shape1, shape2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, 1, length.out = 500), y = pbeta(q = x, shape1, shape2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_cauchy_plot <- function(input, values, prefix_id, type, plot_engine) {
  location <- input[[paste(prefix_id, "cauchy_location", sep = "_")]]
  scale <- input[[paste(prefix_id, "cauchy_scale", sep = "_")]]
  req(location, scale)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qcauchy(0.01, location, scale), qcauchy(0.99, location, scale), length.out = 500),
                     y = dcauchy(x = x, location, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qcauchy(0.01, location, scale), qcauchy(0.99, location, scale), length.out = 500),
                     y = pcauchy(q = x, location, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_chisq_plot <- function(input, values, prefix_id, type, plot_engine) {
  df <- input[[paste(prefix_id, "chisq_df", sep = "_")]]
  req(df)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qchisq(0.99, df), length.out = 500), y = dchisq(x = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qchisq(0, df), qchisq(0.99, df), length.out = 500), y = pchisq(q = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_exp_plot <- function(input, values, prefix_id, type, plot_engine) {
  rate <- input[[paste(prefix_id, "exp_rate", sep = "_")]]
  req(rate)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qexp(0.99, rate), length.out = 500), y = dexp(x = x, rate))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, qexp(0.99, rate), length.out = 500), y = pexp(q = x, rate))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_f_plot <- function(input, values, prefix_id, type, plot_engine) {
  df1 <- input[[paste(prefix_id, "f_df1", sep = "_")]]
  df2 <- input[[paste(prefix_id, "f_df2", sep = "_")]]
  req(df1, df2)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qf(0.99, df1, df2), length.out = 500), y = df(x = x, df1, df2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, qf(0.99, df1, df2), length.out = 500), y = pf(q = x, df1, df2))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_gamma_plot <- function(input, values, prefix_id, type, plot_engine) {
  shape <- input[[paste(prefix_id, "gamma_shape", sep = "_")]]
  scale <- input[[paste(prefix_id, "gamma_scale", sep = "_")]]
  req(shape, scale)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qgamma(0.01, shape = shape, scale = scale), qgamma(0.99, shape = shape, scale = scale), length.out = 500),
                     y = dgamma(x = x, shape = shape, scale = scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qgamma(0.01, shape = shape, scale = scale), qgamma(0.99, shape = shape, scale = scale), length.out = 500),
                     y = pgamma(q = x, shape = shape, scale = scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_lnorm_plot <- function(input, values, prefix_id, type, plot_engine) {
  meanlog <- input[[paste(prefix_id, "lnorm_meanlog", sep = "_")]]
  sdlog <- input[[paste(prefix_id, "lnorm_sdlog", sep = "_")]]
  req(meanlog, sdlog)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(0, qlnorm(0.99, meanlog, sdlog), length.out = 500),
                     y = dlnorm(x = x, meanlog, sdlog))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(0, qlnorm(0.99, meanlog, sdlog), length.out = 500),
                     y = plnorm(q = x, meanlog, sdlog))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_norm_plot <- function(input, values, prefix_id, type, plot_engine) {
  mean <- input[[paste(prefix_id, "norm_mean", sep = "_")]]
  sd <- input[[paste(prefix_id, "norm_sd", sep = "_")]]
  req(mean, sd)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qnorm(0.01, mean, sd), qnorm(0.99, mean, sd), length.out = 500),
                     y = dnorm(x = x, mean, sd))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qnorm(0.01, mean, sd), qnorm(0.99, mean, sd), length.out = 500),
                     y = pnorm(q = x, mean, sd))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_t_plot <- function(input, values, prefix_id, type, plot_engine) {
  df <- input[[paste(prefix_id, "t_df", sep = "_")]]
  req(df)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qt(0.01, df), qt(0.99, df), length.out = 500),
                     y = dt(x = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(qt(0.01, df), qt(0.99, df), length.out = 500),
                     y = pt(q = x, df))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_unif_plot <- function(input, values, prefix_id, type, plot_engine) {
  min <- input[[paste(prefix_id, "unif_min", sep = "_")]]
  max <- input[[paste(prefix_id, "unif_max", sep = "_")]]
  req(min, max)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(min - 1, max + 1, length.out = 500),
                     y = dunif(x = x, min, max))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- tibble(x = seq(min - 1, max + 1, length.out = 500),
                     y = punif(q = x, min, max))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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

get_weibull_plot <- function(input, values, prefix_id, type, plot_engine) {
  shape <- input[[paste(prefix_id, "weibull_shape", sep = "_")]]
  scale <- input[[paste(prefix_id, "weibull_scale", sep = "_")]]
  req(shape, scale)
  if (plot_engine == "ggplot2") {
    if (type == "density") {
      data <- tibble(x = seq(qweibull(0.01, shape, scale), qweibull(0.99, shape, scale), length.out = 500),
                     y = dweibull(x = x, shape, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
        theme_bw()
    } else if (type == "probability") {
      data <- dplyr::tibble(x = seq(qweibull(0.01, shape, scale), qweibull(0.99, shape, scale), length.out = 500),
                     y = pweibull(q = x, shape, scale))
      plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
        geom_line(col = values$einstellungen$ggplot2$col) +
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
