module_verteilungen_input_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(
      outputId = ns("input_rows")
    ),
    verbatimTextOutput(
      outputId = ns("printer")
    )
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

module_verteilungen_input_add_plot_button <- function(id) {
  ns <- NS(id)
  
  actionButton(
    inputId = ns("add_plot"),
    label = label_lang(
      de = "Neuer Plot",
      en = "Add plot"
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
    nrow = 1,
    nplot = 0
  )
  
  observeEvent(input$add_row, {
    rvs$nrow <- rvs$nrow + 1
  })
  
  observeEvent(input$add_plot, {
    rvs$nplot <- rvs$nplot + 1
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = paste0("Verteilung: ", rvs$nplot),
          en = paste0("Distribution: ", rvs$nplot)
        ),
        plotlyOutput(
          outputId = ns("distribution_plot" %_% rvs$nplot)
        ),
        value = ns("plot" %_% rvs$nplot)
      )
    )
    output[["plot" %_% rvs$nplot]] <- renderPlot({
      return(distribution_plot())
    })
  })
  
  distribution_plot <- reactive({
    input_table <- input_table()
    xmax_rows <- input_table[, name == "xmax"]
    xmax_indices <- xmax_rows$index
    for (i in unique(input_table$index)) {
      if (i %in% xmax_indices) {
        
      } else {
        
      }
    }
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
  
  input_table <- reactive({
    data_list <- list()
    for (i in seq_len(rvs$nrow)) {
      data_list[[i]] <- get_arg_values(
        session = session,
        distribution = req(input[["select_distribution" %_% i]]),
        index = i
      )
    }
    data <- rbindlist(data_list)
    print(data)
  })
  
  output$printer <- renderText({
    input_table()
    distribution_plot()
    return("Printer")
  })
  
}