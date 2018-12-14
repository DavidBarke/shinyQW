module_verteilungen_input_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(
    outputId = ns("input_rows")
  )
}

module_verteilungen_input_add_row_button <- function(id) {
  ns <- NS(id)
  
  actionButton(
    inputId = ns("add_row"),
    label = label_lang(
      de = "Neue Zeile",
      en = "Add row"
    )
  )
}

module_verteilungen_input <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("verteilungen_input", parent, session)
  
  ns <- session$ns
  
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
  
  rvs <- reactiveValues(
    nrow = 1
  )
  
  observeEvent(input$add_row, {
    rvs$nrow <- rvs$nrow + 1
  })
  
  output$input_rows <- renderUI({
    ui <- tagList()
    selected_distributions <- list(rvs$nrow)
    for (i in seq_len(rvs$nrow)) {
      selected_distributions[i] <- list(input[["select_distribution" %_% i]])
    }
    for (i in seq_len(rvs$nrow)) {
      ui[[i]] <- fluidRow(
        column(
          width = 4,
          selectInput(
            inputId = ns("select_distribution" %_% i),
            label = label_lang(
              de = "Verteilung",
              en = "Distribution"
            ),
            choices = label_lang_list(
              de = c("Diskrete Verteilungen", "Stetige Verteilungen"),
              en = c("Discrete distributions", "Contiuous distributions"),
              value = list(diskrete_verteilungen, stetige_verteilungen)
            ),
            selected = fallback(selected_distributions[[i]], "binom")
          )
        ),
        column(
          width = 8,
          get_specific_distribution_input(
            session = session, 
            input = input, 
            .values = .values, 
            index = i, 
            distribution = fallback(selected_distributions[[i]], "binom")
          )
        )
      )
    }
    ui
  })
}