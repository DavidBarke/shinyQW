dqe_verteilungsmodelle_zufallsstreubereiche_box <- function(id) {
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
  
  tagList(
    fluidRow(
      column(
        width = 4,
        radioButtons(
          inputId = ns("nseitig"),
          label = label_lang(
            de = "Art",
            en = "Type"
          ),
          choices = label_lang_list(
            de = c("Einseitig", "Zweiseitig"),
            en = c("One-sided", "Two-sided"),
            value = c("one_sided", "two_sided")
          ),
          inline = TRUE
        )
      ),
      column(
        width = 4,
        numericInput(
          inputId = ns("alpha"),
          label = label_lang(
            de = "Signifikanzniveau",
            en = "Level of significance"
          ),
          value = 0.05,
          min = 0,
          max = 1
        )
      ),
      column(
        width = 4,
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
      )
    ),
    uiOutput(
      outputId = ns("specific_distribution_input")
    ),
    fluidRow(
      column(
        width = 4,
        actionButton(
          inputId = ns("add_plot"),
          label = label_lang(
            de = "Plot",
            en = "Plot"
          )
        )
      )
    )
  )
}

dqe_verteilungsmodelle_zufallsstreubereiche <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("zufallsstreubereiche", parent, session)
  
  ns <- session$ns
  
  output$specific_distribution_input <- renderUI({
    get_specific_distribution_input(
      session = session,
      input = input,
      .values = .values,
      prefix_id = NULL,
      distribution = input$distribution
    )
  })
  
  observeEvent(input$add_plot, {
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = "Zufallsstreubereich",
          en = "Random scattering range"
        ),
        plotlyOutput(
          outputId = ns("plot_zufallsstreubereich")
        )
      )
    )
  })
  
  output$plot_zufallsstreubereich <- renderPlotly({
    p <- plot_ly(data = data, x = ~x, y = ~density, type = "scatter", 
                 mode = "lines", fill = "tozeroy") %>%
      layout(
        xaxis = list(
          title = "x"
        ),
        yaxis = list(
          title = "f(x)"
        )
      )
    return(p)
  })
}