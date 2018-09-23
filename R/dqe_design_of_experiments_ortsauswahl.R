#' @export
dqe_design_of_experiments_projekt_ortsauswahl_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      uiOutput(
        outputId = ns("select_data")
      ),
      textInput(
        inputId = ns("toleranzbreite"),
        label = "Toleranzbreite in s:",
        value = 0.5
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabset_main_ortsauswahl"),
        tabPanel(
          title = "Übersicht",
          DT::dataTableOutput(
            outputId = ns("show_data")
          ),
          textOutput(
            outputId = ns("test")
          )
        ),
        tabPanel(
          title = "Kennzahlen",
          fluidRow(
            tags$h4(class = "header", "Kennzahlen nach Orten gruppiert"),
            DT::dataTableOutput(
              outputId = ns("show_kennzahlen")
            )
          ),
          fluidRow(
            tags$h4(class = "header", "Zusammengefasste Kennzahlen"),
            DT::dataTableOutput(
              outputId = ns("show_kennzahlen_zusammengefasst")
            )
          )
        ),
        tabPanel(
          title = "Visualisierungen",
          tabsetPanel(
            id = ns("tabset_visualisierungen"),
            tabPanel(
              title = "Histogramm",
              checkboxInput(
                inputId = ns("histogramm_zusammengefasst"),
                label = "Orte zusammengefasst",
                value = TRUE
              ),
              plotlyOutput(
                outputId = ns("histogramm")
              )
            )
          )
        )
      )
    )
  )
}

#' @export
dqe_design_of_experiments_projekt_ortsauswahl <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                                          parent, ...) {

  self <- node$new("ortsauswahl", parent, session)

  ns <- session$ns

  # Reactives

  data <- reactive({
    if (length_data_storage() != 0) {
      data <- user_data_storage[[input$select_data]]
    }
  })

  length_data_storage <- reactive({
    return(length(names(user_data_storage)))
  })

  # Output

  output$select_data <- renderUI({
    selectInput(
      inputId = ns("select_data"),
      label = "Wähle Datensatz",
      choices = names(user_data_storage),
      selected = if ("Ortsauswahl" %in% names(user_data_storage)) {names(user_data_storage)[str_detect(names(user_data_storage), "Ortsauswahl")]}
    )
  })

  # Struktur der Daten
  # Spalten: Wurf, Ort, Datum, Abwurfhoehe, Flugdauer

  # Übersicht

  output$show_data <- DT::renderDataTable({
    if (length_data_storage() != 0) {
      datatable(data())
    }
  })

  # Kennzahlen für Orte

  output$show_kennzahlen <- DT::renderDataTable({
    if (length_user_data_storage() != 0) {
      toleranzbreite <- as.numeric(input$toleranzbreite)
      data <- data() %>%
        group_by(Ort) %>%
        summarise(`Durchschnittliche Flugdauer` = mean(Flugdauer),
                  `Standardabweichung` = sqrt((n() - 1)/(n())) * sd(Flugdauer),
                  `cp-Wert` = toleranzbreite / (6 * sqrt((n() - 1) / (n())) * sd(Flugdauer)))
      data <- datatable(data)
    }
  })

  output$show_kennzahlen_zusammengefasst <- DT::renderDataTable({
    if (user_data_storage() != 0) {
      toleranzbreite <- as.numeric(input$toleranzbreite)
      data <- data() %>%
        summarise(`Durchschnittliche Flugdauer` = mean(Flugdauer),
                  `Standardabweichung` = sqrt((n() - 1)/ (n())) * sd(Flugdauer),
                  `cp-Wert` = toleranzbreite / (6 * sqrt((n() - 1) / (n())) * sd(Flugdauer)))
      data <- datatable(data)
    }
  })

  # Visualisierungen

  ## Histogramm

  output$histogramm <- renderPlotly({
    if (input$histogramm_zusammengefasst) {
      plot <- histogramm_zusammengefasst()
    } else {
      plot <- histogramm_orte()
    }
    return(plot)
  })

  histogramm_zusammengefasst <- reactive({
    toleranzbreite <- as.numeric(input$toleranzbreite)
    flugdauer <- data()$Flugdauer
    k <- floor(sqrt(length(flugdauer)))
    mean_flugdauer <- mean(flugdauer)
    sd_flugdauer <- sd(flugdauer)
    min <- min(flugdauer, na.rm = TRUE)
    max <- max(flugdauer, na.rm = TRUE)
    R <- max - min
    b <- R / k
    breaks <- min + 0:k * b
    data_norm <- data.frame(x = seq(min, max, length.out = 100)) %>%
      mutate(y = dnorm(x = x, mean = mean_flugdauer, sd = sd_flugdauer))
    plot <- ggplot(data = data.frame(Flugdauer = flugdauer)) +
      geom_histogram(mapping = aes(x = Flugdauer, y = ..density..), breaks = breaks, closed = "left", col = "mediumblue", fill = "lightblue") +
      geom_vline(xintercept = mean_flugdauer - toleranzbreite/2, col = "red", linetype = "dashed") +
      geom_vline(xintercept = mean_flugdauer + toleranzbreite/2, col = "red", linetype = "dashed") +
      geom_line(data = data_norm, mapping = aes(x = x, y = y)) +
      labs(x = "Flugdauer", y = "Relative Häufigkeitsdichte") +
      theme_bw()
    plot <- ggplotly(plot)
    return(plot)
  })

  histogramm_orte <- reactive({

  })
}
