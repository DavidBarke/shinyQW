#' @export
dqe_deskriptive_statistik_gruppierte_daten_box <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      inputId = ns("input_type"),
      label = "Datenquelle",
      choices = list("Zufallszahlen" = "random",
                     "Daten" = "data")
    ),
    uiOutput(
      outputId = ns("specific_type_input")
    ),
    fluidRow(
      column(
        width = 6,
        actionButton(
          inputId = ns("update"),
          label = "Aktualisiere"
        )
      )
    )
  )
}

#' @export
dqe_deskriptive_statistik_gruppierte_daten <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("gruppierte_daten", parent, session)

  ns <- session$ns
  
  rvs <- reactiveValues(
    counter = 0
  )
  
  output$specific_type_input <- renderUI({
    if (input$input_type == "random") {
      ui <- div(
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("random_type"),
              label = label_lang(
                de = "Typ",
                en = "Type"
              ),
              choices = label_lang_list(
                de = c("Integer", "Stetig"),
                en = c("Integer", "Continous"),
                value = c("integer", "continous")
              )
            )
          ),
          column(
            width = 6,
            sliderInput(
              inputId = ns("min_max"),
              label = label_lang(
                de = "Min und Max",
                en = "Min and max"
              ),
              value = c(0, 10),
              min = 0,
              max = 100
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = ns("laenge"),
              label = label_lang(
                de = "Anzahl",
                en = "Count"
              ),
              value = 10,
              min = 1,
              max = 100
            )
          )
        )
      )
    } else if (input$input_type == "data") {
      ui <- data_selector_default_ui(
        id = ns("id_data_selector"),
        column = "single"
      )
    }
  })

  k <- reactive({
    return(nclass.Sturges(req(x_data())))
  })

  min_x_data <- reactive({
    return(min(req(x_data()), na.rm = TRUE))
  })

  max_x_data <- reactive({
    return(max(req(x_data()), na.rm = TRUE))
  })

  R <- reactive({
    req(x_data())
    return(max_x_data() - min_x_data())
  })

  b <- reactive({
    req(x_data())
    b <- as.numeric(format(R() / k()))
    b <- round(b, digits = 3)
    return(b)
  })

  breaks <- reactive({
    req(x_data())
    return(min_x_data() + 0:k() * b())
  })

  groups <- reactive({
    cut(req(x_data()), breaks = breaks(), right = FALSE, ordered_result = TRUE,
        dig.lab = 4, include.lowest = TRUE)
  })

  tabellierte_haeufigkeitsverteilung <- reactive({
    data <- table_frequency_distribution(x = req(x_data()),
                                              b = b(),
                                              k = k())
    return(data)
  })

  output$tabellierte_haeufigkeitsverteilung <- DT::renderDataTable({
    req(x_data())
    data <- DT::datatable(tabellierte_haeufigkeitsverteilung(),
                          options = list(scrollX = TRUE))
    return(data)
  })

  histogramm_relativ <- reactive({
    plot <- histogram(data = tabellierte_haeufigkeitsverteilung(),
                           frequency_density = "relative",
                           breaks = breaks(),
                           col = .values$einstellungen$ggplot2$col,
                           fill = .values$einstellungen$ggplot2$fill,
                           alpha = .values$einstellungen$ggplot2$alpha)
    return(plot)
  })

  histogramm_absolut <- reactive({
    plot <- histogram(data = tabellierte_haeufigkeitsverteilung(),
                           frequency_density = "absolute",
                           breaks = breaks(),
                           col = .values$einstellungen$ggplot2$col,
                           fill = .values$einstellungen$ggplot2$fill,
                           alpha = .values$einstellungen$ggplot2$alpha)
    return(plot)
  })

  verteilungsfunktion <- reactive({
    plot <- cumulative_distribution_function(tabellierte_haeufigkeitsverteilung(),
                                             breaks = breaks(),
                                             col = .values$einstellungen$ggplot2$col,
                                             fill = .values$einstellungen$ggplot2$fill,
                                             alpha = .values$einstellungen$ggplot2$alpha,
                                             size = .values$einstellungen$ggplot2$size)
    return(plot)
  })

  output$plot_group <- renderPlot({
    plot <- histogramm_absolut() + histogramm_relativ() -
      verteilungsfunktion() +
      plot_layout(ncol = 1)
    return(plot)
  })

  x_data <- reactive({
    rvs$counter
    isolate({
      if (input$input_type == "random") {
        x_data <- floor(runif(n = fallback(input$laenge, 10),
                              min = fallback(input$min_max[1], 0),
                              max = fallback(input$min_max[2] + 1, 11)))
      } else if (input$input_type == "data") {
        x_data <- selected_col$col_val()
      }
    })
    return(x_data)
  })

  observeEvent(input$update, {
    rvs$counter <- rvs$counter + 1
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = "Tabellierte Häufigkeitsverteilung",
          en = "Tabulared frequency table"
        ),
        DT::dataTableOutput(
          outputId = ns("tabellierte_haeufigkeitsverteilung"),
          width = "auto"
        ),
        actionButton(
          inputId = ns("show_plot"),
          label = label_lang(
            de = "Zugehöriger Plot",
            en = "Affiliated plot"
          )
        ),
        value = ns("tabled_frequency_distribution")
      )
    )
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = "Grafische Häufigkeitsverteilung",
          en = "Graphical frequency distribution"
        ),
        plotOutput(
          outputId = ns("plot_group")
        ),
        actionButton(
          inputId = ns("show_table"),
          label = label_lang(
            de = "Zugehörige Daten",
            en = "Affiliated data"
          )
        ),
        value = ns("graphical_frequency_distribution")
      )
    )
  })
  
  observeEvent(input$show_table, {
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = "Tabellierte Häufigkeitsverteilung",
        fluidRow(
          column(
            width = 12,
            DT::dataTableOutput(
              outputId = ns("tabellierte_haeufigkeitsverteilung"),
              width = "auto"
            )
          )
        ),
        value = ns("tabled_frequency_distribution")
      )
    )
  })
  
  observeEvent(input$show_plot, {
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = "Grafische Häufigkeitsverteilung",
          en = "Graphical frequency distribution"
        ),
        plotOutput(
          outputId = ns("plot_group")
        ),
        actionButton(
          inputId = ns("show_table"),
          label = label_lang(
            de = "Zugehörige Daten",
            en = "Affiliated data"
          )
        ),
        value = ns("graphical_frequency_distribution")
      )
    )
  })

  selected_col <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .data = .data,
    .values = .values,
    parent = self
  )
}
