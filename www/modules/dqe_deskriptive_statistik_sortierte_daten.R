#' @export
dqe_deskriptive_statistik_sortierte_daten_box <- function(id) {
  ns <- NS(id)

  tagList(
    selectInput(
      inputId = ns("input_type"),
      label = label_lang(
        de = "Datenquelle",
        en = "Data source"
      ),
      choices = label_lang_list(
        de = c("Zufallszahlen", "Daten"),
        en = c("Random numbers", "Data"),
        value = c("random", "data")
      )
    ),
    uiOutput(
      outputId = ns("specific_type_input")
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
  
  selected_col <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .data = .data,
    .values = .values,
    parent = self
  )

  raw_data <- reactive({
    rvs$counter
    isolate({
      if (input$input_type == "random") {
        raw_data <- runif(n = fallback(input$laenge, 10),
                          min = fallback(input$min_max[1], 0),
                          max = fallback(input$min_max[2] + 1, 11))
        if (input$random_type == "integer") {
          raw_data <- floor(raw_data)
        }
      } else if (input$input_type == "data") {
        raw_data <- selected_col()$col_val
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

  observeEvent(input$update, {
    rvs$counter <- rvs$counter + 1
    .values$viewer$data$append_tab(
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
}
