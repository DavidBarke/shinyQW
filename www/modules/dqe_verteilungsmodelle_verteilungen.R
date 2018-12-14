dqe_verteilungsmodelle_verteilungen_box <- function(id) {
  ns <- NS(id)

  diskrete_verteilungen <- label_lang_list(
    de = c("Binomialverteilung", "Geometrische Verteilung",
           "Hypergeometrische Verteilung", "Multinomialverteilung",
           "Negative Binomialverteilung", "Poissonverteilung"),
    en = c("Binomial distribution", "Geometric Distribution",
           "Hypergeometric distribution", "Multinomial distribution",
           "Negative binomial distribution", "Poisson distribution"),
    value = c("binom", "geom", "hyper", "multinom", "nbinom", "pois")
  )

  stetige_verteilungen <- label_lang_list(
    de = c("Betaverteilung", "Cauchyverteilung",
           "Chi-Quadrat-Verteilung", "Exponentialverteilung",
           "F-Verteilung", "Gammaverteilung",
           "Gleichverteilung", "Log-Normalverteilung",
           "Normalverteilung", "t-Verteilung",
           "Weibullverteilung"),
    en = c("Beta distribution", "Cauchy distribution",
           "Chi-squared distribution", "Exponential distribution",
           "F-distribution", "Gamma distribution",
           "Uniform distribution", "Log-normal distribution",
           "Normal distribution", "Student's t-distribution",
           "Weibull distribution"),
    value = c("beta", "cauchy", "chisq", "exp", "f", "gamma", "unif", "lnorm",
              "norm", "t", "weibull")
  )

  shiny::tagList(
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("distribution"),
          label = label_lang(
            de = "Verteilung",
            en = "Distribution"
          ),
          choices = label_lang_list(
            de = c("Diskrete Verteilungen", "Stetige Verteilungen"),
            en = c("Discrete distributions", "Contiuous distributions"),
            value = list(diskrete_verteilungen, stetige_verteilungen)
          )
        )
      ),
      column(
        width = 6,
        selectInput(
          inputId = ns("type"),
          label = label_lang(
            de = "Art des Plots",
            en = "Plot type"
          ),
          choices = label_lang_list(
            de = c("Dichtefunktion", "Verteilungsfunktion", "Quantilsfunktion"),
            en = c("Density function", "Probability Function", "Quantile function"),
            value = c("density", "probability", "quantile")
          )
        )
      )
    ),
    uiOutput(
      outputId = ns("specific_input")
    ),
    actionButton(
      inputId = ns("add_verteilungen_plot"),
      label = label_lang(
        de = "Öffne Plot",
        en = "Add Plot"
      )
    )
  )
}

dqe_verteilungsmodelle_verteilungen <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("verteilungen", parent, session)

  ns <- session$ns

  rvs <- reactiveValues(
    counter = 0
  )

  observeEvent(input$add_verteilungen_plot, {
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = input$distribution,
        plotOutput(
          outputId = ns("plot" %_% rvs$counter)
        ),
        value = ns(input$distribution)
      )
    )
    output[["plot" %_% rvs$counter]] <- renderPlot({
      plot <- get_distribution_plot(
        input = input,
        .values = .values,
        prefix_id = NULL,
        distribution = input$distribution,
        type = input$type,
        plot_engine = "ggplot2"
      )
      return(plot)
    })
    rvs$counter <- rvs$counter + 1
  })

  output$specific_input <- renderUI({
    get_specific_distribution_input(
      session = session,
      input = input,
      .values = .values,
      index = NULL,
      distribution = input$distribution
    )
  })
}
