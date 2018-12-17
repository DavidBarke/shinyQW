# TODO: unique(indices) durch seq_len(max(indices)) ersetzen

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
    output[["distribution_plot" %_% rvs$nplot]] <- renderPlotly({
      return(distribution_plot())
    })
  })
  
  distribution_plot <- reactive({
    p_min = 0.01
    p_max = 0.99
    input_table <- input_table()
    input_short_table <- input_short_table()
    indices <- input_table$index
    xmax_rows <- input_table[name == "xmax"]
    xmax_indices <- xmax_rows$index
    x_min <- numeric(length(unique(indices)))
    x_max <- numeric(length(unique(indices)))
    for (i in seq_len(max(indices))) {
      subset_table <- input_table[index == i]
      if (i %in% xmax_indices) {
        arg_values <- subset_table[name != "xmax", value]
        names(arg_values) <- subset_table[name != "xmax", name]
        subset_args <- as.list(arg_values)
        x_min[i] <- 0
        x_max[i] <- subset_table[name == "xmax", value]
      } else {
        arg_values <- subset_table[, value]
        names(arg_values) <- subset_table[, name]
        subset_args <- as.list(arg_values)
        if (any(subset_table[, discrete])) {
          x_min[i] <- 0
          x_max[i] <- do.call(
            what = paste0("q", input_short_table[i, distribution]),
            args = c(list(p = 1), subset_args)
          )
        } else {
          x_min[i] <- do.call(
            what = paste0("q", input_short_table[i, distribution]),
            args = c(list(p = p_min), subset_args)
          )
          x_max[i] <- do.call(
            what = paste0("q", input_short_table[i, distribution]),
            args = c(list(p = p_max), subset_args)
          )
        }
      }
    }
    x_min_min <- floor(min(x_min))
    x_max_max <- ceiling(max(x_max))
    x_int <- x_min_min:x_max_max
    data <- tibble(x = x_int)
    for (i in seq_len(max(indices))) {
      subset_table <- input_table[index == i]
      arg_values <- subset_table[name != "xmax", value]
      names(arg_values) <- subset_table[name != "xmax", name]
      subset_args <- as.list(arg_values)
      data[["y" %_% i]] <- do.call(
        what = paste0("d", input_short_table[i, distribution]),
        args = c(list(x = x_int), subset_args)
      )
      # Für den Fall, dass zwischen x_int und x_seq für diskrete bzw. stetige
      # Verteilungen unterschieden wird
      # assign(
      #   "data" %_% i, 
      #   data.table(
      #     x = x_int,
      #     y = do.call(
      #       what = paste0("d", subset_table[, distribution][1]),
      #       args = c(list(x = x_int), subset_args)
      #     )
      #   )
      # )
    }
    p <- plot_ly(data = data, x = ~x)
    for (i in seq_len(max(indices))) {
      discrete = input_short_table[i, discrete]
      if (discrete) {
        type <- "bar"
      } else {
        type <- "scatter"
      }
      p <- add_trace(
        p = p,
        y = data[["y" %_% i]],
        name = input_table[index == i, distribution][1] %_% i,
        type = type,
        mode = "lines"
      )
    }
    p
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
  })
  
  input_short_table <- reactive({
    input_table <- input_table()
    input_short_table <- input_table[,.(index, distribution, discrete)][
      , head(.SD, 1), by = index
    ]
  })
  
  output$printer <- renderText({
    input_table()
    distribution_plot()
    return("Printer")
  })
  
}