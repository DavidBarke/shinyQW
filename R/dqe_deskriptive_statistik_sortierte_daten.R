#' @export
dqe_deskriptive_statistik_sortierte_daten_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("tabset"),
    tabPanel(
      title = "",
      value = ns("tabset_default"),
      div(
        id = ns("sortierte_daten"),
        fluidRow(
          column(
            id = ns("div_input"),
            width = 4,
            fluidRow(
              column(
                width = 6,
                selectInput(
                  inputId = ns("input_type"),
                  label = "Datenquelle",
                  choices = list("Zufallszahlen" = "random",
                                 "Daten" = "data_storage"),
                  selected = "random"
                )
              ),
              column(
                id = ns("random_type_container"),
                width = 6,
                selectInput(
                  inputId = ns("random_type"),
                  label = "Typ",
                  choices = list(Integer = "integer",
                                 Stetig = "stetig")
                )
              )
            ),
            div(
              id = ns("input_type_input")
            ),
            fluidRow(
              column(
                width = 6,
                actionButton(
                  inputId = ns("update"),
                  label = "Aktualisiere"
                )
              ),
              column(
                width = 6
              )
            )
          ),
          column(
            id = ns("tabellarische_haeufigkeitsverteilung"),
            width = 8,
            DT::dataTableOutput(
              outputId = ns("datatable")
            )
          )
        ),
        fluidRow(
          id = ns("plots"),
          fluidRow(
            column(
              width = 6,
              id = ns("container_plot_h_aj"),
              plotOutput(
                outputId = ns("plot_h_aj")
              )
            ),
            column(
              width = 6,
              id = ns("container_plot_f_aj"),
              plotOutput(
                outputId = ns("plot_f_aj")
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              id = ns("container_plot_H_x"),
              plotOutput(
                outputId = ns("plot_H_x")
              )
            ),
            column(
              width = 6,
              id = ns("container_plot_F_x"),
              plotOutput(
                outputId = ns("plot_F_x")
              )
            )
          )
        )
      )
    )
  )
}

#' @export
dqe_deskriptive_statistik_sortierte_daten <- function(
  input, output, session, data, values, parent, ...
) {

  self <- node$new("sortierte_daten", parent, session)

  ns <- session$ns

  raw_data <- reactive({
    req(rvs_data$laenge, rvs_data$minmax, rvs_data$counter)
    isolate({
      if (input$input_type == "random") {
        raw_data <- runif(n = rvs_data$laenge,
                          min = rvs_data$minmax[[1]],
                          max = rvs_data$minmax[[2]] + 1)
        if (input$random_type == "integer") {
          raw_data <- floor(raw_data)
        }
      } else if (input$input_type == "data_storage") {
        isolate({
          select_data <- call_select_data()$values
          print(select_data)
          data_storage <- get(
            x = paste(select_data$data_type, "data_storage", sep = "_"),
            pos = data
          )
          raw_data <- data_storage[[select_data$data$selected]][[select_data$data$column$selected_1]]
        })
      }
    })
    return(raw_data)
  })

  data <- reactive({
    x <- raw_data()
    data <- tibble(a_j = x) %>%
      group_by(a_j) %>%
      summarise(h_aj = n()) %>%
      mutate(f_aj = h_aj / sum(h_aj), H_x = cumsum(h_aj), F_x = cumsum(f_aj))
    return(data)
  })

  # Tabellierte Häufigkeitsverteilung
  rvs_data <- reactiveValues(counter = 0)

  observeEvent(input$update, {
    rvs_data$counter <- rvs_data$counter + 1
    rvs_data$laenge <- input$laenge
    rvs_data$minmax <- input$minmax
  })

  output$datatable <- DT::renderDataTable({
    data <- req(data())
    data <- DT::datatable(data)
    return(data)
  })

  # Grafische Häufigkeitsverteilung
  base_plot <- reactive({
    data <- req(raw_data())
    plot <- tibble(a_j = data) %>%
      ggplot(., mapping = aes(x = a_j)) +
      scale_x_continuous(breaks = sort(unique(data)),
                         labels = sort(unique(data))) +
      theme_bw()
    return(plot)
  })

  output$plot_h_aj <- renderPlot({
    base_plot <- base_plot()
    plot <- base_plot +
      geom_bar(mapping = aes(y = ..count..),
               col = values$einstellungen$ggplot2$col,
               fill = values$einstellungen$ggplot2$fill,
               alpha = values$einstellungen$ggplot2$alpha) +
      labs(x = "", y = expression(h(a[j])))
    return(plot)
  })

  output$plot_f_aj <- renderPlot({
    base_plot <- base_plot()
    plot <- base_plot +
      geom_bar(mapping = aes(y = ..count.. / sum(..count..)),
               col = values$einstellungen$ggplot2$col,
               fill = values$einstellungen$ggplot2$fill,
               alpha = values$einstellungen$ggplot2$alpha) +
      labs(x = "", y = expression(f(a[j])))
    return(plot)
  })

  output$plot_H_x <- renderPlot({
    base_plot <- base_plot()
    plot <- base_plot +
      stat_count(aes(y = cumsum(..count..)),
                 geom = "step",
                 col = values$einstellungen$ggplot2$col,
                 alpha = values$einstellungen$ggplot2$alpha) +
      labs(x = "", y = expression(H(x)))
    return(plot)
  })

  output$plot_F_x <- renderPlot({
    base_plot <- base_plot()
    plot <- base_plot +
      stat_count(aes(y = cumsum(..count.. / sum(..count..))),
                 geom = "step",
                 col = values$einstellungen$ggplot2$col,
                 alpha = values$einstellungen$ggplot2$alpha) +
      labs(x = "", y = expression(F(x)))
    return(plot)
  })

  # Spezifischer Input für sortierte Daten
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
          select_data_ui(id = ns("id_sortierte_daten_select_data"))
        )
      }
      # Insert UI-Element
      insertUI(
        selector = paste0("#", ns(insertDivId)),
        where = "beforeEnd",
        ui = ui_element
      )
    }
    if (input$input_type == "random") {
      shinyjs::show(
        selector = paste0("#", ns("random_type_container"))
      )
    } else if (input$input_type == "data_storage") {
      shinyjs::hide(
        selector = paste0("#", ns("random_type_container"))
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
                                id = "id_sortierte_daten_select_data",
                                data_rvs = data,
                                values = values,
                                parent = self,
                                tabset_data = tibble(
                                  id = c("tabset", "tabset"),
                                  session = c(session, session),
                                  type = c("append_new_tab", "append_current_tab"),
                                  position = c(1, 2),
                                  label = c("Im neuen Tab anzeigen", "Im gegenwärtigen Tab anzeigen")
                                ),
                                select_column = TRUE,
                                grid_select = tibble(type = "select", position = 1, label = "Wähle Spalte"))
}
