#' @export
dqe_deskriptive_statistik_sortierte_daten_box <- function(id, .language) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("input_type"),
          label = label_lang(
            de = "Datenquelle",
            en = "Data source"
          ),
          choices = label_lang_list(
            de = c("Zufallszahlen", "Daten"),
            en = c("Random numbers", "Data"),
            value = c("random", "data_storage")
          ),
          selected = "random"
        )
      ),
      column(
        id = ns("random_type_container"),
        width = 6,
        selectInput(
          inputId = ns("random_type"),
          label = label_lang(
            de = "Typ",
            en = "Type"
          ),
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
          label = label_lang(
            de = "Aktualisiere",
            en = "Update"
          )
        )
      ),
      column(
        width = 6
      )
    )
  )
}

#' @export
dqe_deskriptive_statistik_sortierte_daten <- function(
  input, output, session, .data, .values, parent, ...
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
          data_storage <- get(
            select_data$data_type,
            pos = .data
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
    .values$viewer$data$appendTab(
      tab = tabPanel(
        title = "Tabellierte Häufigkeitsverteilung",
        DT::dataTableOutput(
          outputId = ns("datatable")
        ),
        value = ns("Tabellarische_Häufigkeitsverteilung")
      ),
      select = TRUE
    )
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = "Grafische Häufigkeitsverteilung",
        plotOutput(
          outputId = ns("plot_group")
        ),
        value = ns("Grafische_Häufigkeitsverteilung")
      ),
      select = TRUE
    )
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



  plot_h_aj <- reactive({
    base_plot <- base_plot()
    plot <- base_plot +
      geom_bar(mapping = aes(y = ..count..),
               col = .values$einstellungen$ggplot2$col,
               fill = .values$einstellungen$ggplot2$fill,
               alpha = .values$einstellungen$ggplot2$alpha
               ) +
      labs(x = "", y = expression(h(a[j])))
    return(plot)
  })

  plot_f_aj <- reactive({
    base_plot <- base_plot()
    plot <- base_plot +
      geom_bar(mapping = aes(y = ..count.. / sum(..count..)),
               col = .values$einstellungen$ggplot2$col,
               fill = .values$einstellungen$ggplot2$fill,
               alpha = .values$einstellungen$ggplot2$alpha
               ) +
      labs(x = "", y = expression(f(a[j])))
    return(plot)
  })

  plot_H_x <- reactive({
    base_plot <- base_plot()
    plot <- base_plot +
      stat_count(aes(y = cumsum(..count..)),
                 geom = "step",
                 col = .values$einstellungen$ggplot2$col,
                 alpha = .values$einstellungen$ggplot2$alpha
                 ) +
      labs(x = "", y = expression(H(x)))
    return(plot)
  })

  plot_F_x <- reactive({
    base_plot <- base_plot()
    plot <- base_plot +
      stat_count(aes(y = cumsum(..count.. / sum(..count..))),
                 geom = "step",
                 col = .values$einstellungen$ggplot2$col,
                 alpha = .values$einstellungen$ggplot2$alpha
                 ) +
      labs(x = "", y = expression(F(x)))
    return(plot)
  })

  output$plot_group <- renderPlot({
    plot <- plot_h_aj() + plot_f_aj() + plot_H_x() + plot_F_x() +
      patchwork::plot_layout(
        ncol = 2, nrow = 2
      )
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
                                data_rvs = .data,
                                .values = .values,
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