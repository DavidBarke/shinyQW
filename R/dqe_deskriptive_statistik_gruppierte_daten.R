#' @export
dqe_deskriptive_statistik_gruppierte_daten_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tabsetPanel(
    id = ns("tabset"),
    shiny::tabPanel(
      title = "",
      value = ns("tabset_default"),
      htmltools::div(
        id = ns("gruppierte_daten"),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectInput(
                  inputId = ns("input_type"),
                  label = "Datenquelle",
                  choices = list("Zufallszahlen" = "random",
                                 "Daten" = "data_storage"),
                  selected = "random"
                )
              )
            ),
            htmltools::div(
              id = ns("input_type_input")
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::actionButton(
                  inputId = ns("update"),
                  label = "Aktualisiere"
                )
              )
            )
          ),
          shiny::column(
            width = 8,
            shiny::fluidRow(
              DT::dataTableOutput(
                outputId = ns("tabellierte_haeufigkeitsverteilung")
              )
            )
          )
        ),
        shiny::fluidRow(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::plotOutput(
                outputId = ns("histogramm_absolut")
              )
            ),
            shiny::column(
              width = 6,
              shiny::plotOutput(
                outputId = ns("histogramm_relativ")
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::plotOutput(
                outputId = ns("verteilungsfunktion")
              )
            ),
            shiny::column(
              width = 6
            )
          )
        )
      )
    )
  )
}

#' @export
dqe_deskriptive_statistik_gruppierte_daten <- function(
  input, output, session, data, values, parent, ...
) {
  self <- node$new("gruppierte_daten", parent, session)

  ns <- session$ns

  k <- shiny::reactive({
    return(nclass.Sturges(req(x_data())))
  })

  min_x_data <- shiny::reactive({
    return(min(req(x_data()), na.rm = TRUE))
  })

  max_x_data <- shiny::reactive({
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
    data <- shinyQW::table_frequency_distribution(x = req(x_data()),
                                              b = b(),
                                              k = k())
    return(data)
  })

  output$tabellierte_haeufigkeitsverteilung <- DT::renderDataTable({
    req(x_data())
    data <- DT::datatable(tabellierte_haeufigkeitsverteilung())
    return(data)
  })

  output$histogramm_relativ <- renderPlot({
    plot <- shinyQW::histogram(data = tabellierte_haeufigkeitsverteilung(),
                           frequency_density = "relative",
                           breaks = breaks(),
                           col = values$einstellungen$ggplot2$col,
                           fill = values$einstellungen$ggplot2$fill,
                           alpha = values$einstellungen$ggplot2$alpha)
    return(plot)
  })

  output$histogramm_absolut <- renderPlot({
    plot <- shinyQW::histogram(data = tabellierte_haeufigkeitsverteilung(),
                           frequency_density = "absolute",
                           breaks = breaks(),
                           col = values$einstellungen$ggplot2$col,
                           fill = values$einstellungen$ggplot2$fill,
                           alpha = values$einstellungen$ggplot2$alpha)
    return(plot)
  })

  output$verteilungsfunktion <- renderPlot({
    plot <- cumulative_distribution_function(tabellierte_haeufigkeitsverteilung(),
                                             breaks = breaks(),
                                             col = values$einstellungen$ggplot2$col,
                                             fill = values$einstellungen$ggplot2$fill,
                                             alpha = values$einstellungen$ggplot2$alpha)
    return(plot)
  })

  rvs <- reactiveValues()

  x_data <- reactive({
    return(req(rvs$x_data))
  })

  observeEvent(input$update, {
    req(input$laenge, input$minmax)
    if (input$input_type == "random") {
      x_data <- floor(runif(n = input$laenge,
                            min = input$minmax[[1]],
                            max = input$minmax[[2]] + 1))
    } else if (input$input_type == "data_storage") {
      select_data <- call_select_data()$values
      data_storage <- get(x = paste(select_data$data_type, sep = "_"),
                          pos = data)
      x_data <- data_storage[[select_data$data$selected]][[select_data$data$column$selected_1]]
    }
    rvs$x_data <- x_data
  })

  # Spezifischer Input für gruppierte Daten
  rvs_specific_input <- reactiveValues()

  observeEvent(input$input_type, {
    # IDs
    insertDivId <- "input_type_input"
    uiDivId <- paste("specific_input", input$input_type, sep = "_")
    # Neues UI-Element erzeugen, falls es noch nicht existiert
    if (!input$input_type %in% names(rvs_specific_input)) {
      rvs_specific_input[[input$input_type]] <- 1
      uiDivClass <- "specific_input_class"
      if (input$input_type == "random") {
        ui_element <- div(
          id = ns(uiDivId),
          class = ns(uiDivClass),
          fluidRow(
            column(
              width = 6,
              sliderInput(
                inputId = ns("minmax"),
                label = "Min und Max",
                value = c(0, 10),
                min = 0,
                max = 100
              )
            ),
            column(
              width = 6,
              numericInput(
                inputId = ns("laenge"),
                label = "Anzahl",
                value = 10,
                min = 1,
                max = 100
              )
            )
          )
        )
      } else if (input$input_type == "data_storage") {
        ui_element <- div(
          id = ns(uiDivId),
          class = ns(uiDivClass),
          select_data_ui(id = ns("id_gruppierte_daten_select_data"))
        )
      }
      # Insert UI-Element
      insertUI(
        selector = paste("#", ns(insertDivId), sep = ""),
        where = "beforeEnd",
        ui = ui_element
      )
    }
    shinyjs::hide(
      selector = paste(".", ns("specific_input_class"), sep = "")
    )
    shinyjs::show(
      selector = paste("#", ns(uiDivId), sep = "")
    )
  })

  call_select_data <- callModule(module = select_data,
                                 id = "id_gruppierte_daten_select_data",
                                 data_rvs = data,
                                 values = values,
                                 parent = self,
                                 tabset_data = tibble(
                                   id = c("tabset", "tabset"),
                                   session = c(session, session),
                                   type = c("append_new_tab", "append_current_tab"),
                                   position = c(1, 2),
                                   label = c("Im neuen Tab anzeigen", "Im gegenwärtigen Tab anzeigen")
                                 )
  )
}
