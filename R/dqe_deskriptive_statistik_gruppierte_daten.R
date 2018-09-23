#' @export
dqe_deskriptive_statistik_gruppierte_daten_ui <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    id = ns("tabset"),
    tabPanel(
      title = "",
      value = ns("tabset_default"),
      div(
        id = ns("gruppierte_daten"),
        fluidRow(
          column(
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
              )
            )
          ),
          column(
            width = 8,
            fluidRow(
              DT::dataTableOutput(
                outputId = ns("tabellierte_haeufigkeitsverteilung")
              )
            )
          )
        ),
        fluidRow(
          fluidRow(
            column(
              width = 6,
              plotOutput(
                outputId = ns("histogramm_absolut")
              )
            ),
            column(
              width = 6,
              plotOutput(
                outputId = ns("histogramm_relativ")
              )
            )
          ),
          fluidRow(
            column(
              width = 6,
              plotOutput(
                outputId = ns("verteilungsfunktion")
              )
            ),
            column(
              width = 6
            )
          )
        )
      )
    )
  )
}

#' @export
dqe_deskriptive_statistik_gruppierte_daten <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                                       parent, ...) {
  self <- node$new("gruppierte_daten", parent, session)

  ns <- session$ns

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
    cut(req(x_data()), breaks = breaks(), right = FALSE, ordered_result = TRUE, dig.lab = 4, include.lowest = TRUE)
  })

  get_h_j <- function(levels, occurences) {
    h_j <- vector("numeric", length = length(levels))
    for (i in 1:length(levels)) {
      h_j[i] = sum(occurences == levels[i])
    }
    return(h_j)
  }

  tabellierte_haeufigkeitsverteilung <- reactive({
    req(x_data())
    b <- b()
    k <- k()
    groups <- groups()
    data <- tibble(Klasse = levels(groups))
    data$h_j <- get_h_j(levels(groups), groups)
    data <- data %>%
      mutate(f_j = h_j / sum(h_j), H_x = cumsum(h_j), F_x = cumsum(f_j),
             b_j = b, m_j = ((1:k) - 0.5) * b + min_x_data(),
             h_dichte = h_j / b_j, f_dichte = f_j / b_j)
    return(data)
  })

  output$tabellierte_haeufigkeitsverteilung <- DT::renderDataTable({
    req(x_data())
    data <- DT::datatable(tabellierte_haeufigkeitsverteilung())
    return(data)
  })

  histogramm <- function(type) {
    plot <- reactive({
      if (type == "relativ") {
        dichte <- "f_j"
        y_label <- expression(tilde(f)[j])
      } else if (type == "absolut") {
        dichte <- "h_j"
        y_label <- expression(tilde(h)[j])
      }
      data <- tabellierte_haeufigkeitsverteilung()
      breaks_x <- breaks()
      breaks_y <- data[[dichte]]
      plot <- ggplot(data = data) +
        geom_tile(mapping = aes(x = m_j, y = data[[dichte]]/2, width = b_j, height = data[[dichte]]),
                  col = values$einstellungen$ggplot2$col,
                  fill = values$einstellungen$ggplot2$fill,
                  alpha = values$einstellungen$ggplot2$alpha) +
        scale_x_continuous(breaks = breaks_x, labels = breaks_x) +
        scale_y_continuous(breaks = breaks_y, labels = breaks_y) +
        labs(x = "", y = y_label) +
        theme_bw()
    })
    return(plot())
  }

  output$histogramm_relativ <- renderPlot({
    plot <- histogramm("relativ")
    return(plot)
  })

  output$histogramm_absolut <- renderPlot({
    plot <- histogramm("absolut")
    return(plot)
  })

  output$verteilungsfunktion <- renderPlot({
    tab_haeuf <- tabellierte_haeufigkeitsverteilung()
    breaks <- breaks()
    data <- tibble(x = breaks, y = c(0, tab_haeuf$F_x))
    plot <- ggplot(data = data, mapping = aes(x = x, y = y)) +
      geom_line(col = values$einstellungen$ggplot2$col,
                alpha = values$einstellungen$ggplot2$alpha,
                size = values$einstellungen$ggplot2$size) +
      geom_point(col = values$einstellungen$ggplot2$col,
                 alpha = values$einstellungen$ggplot2$alpha,
                 size = 2 * values$einstellungen$ggplot2$size) +
      scale_x_continuous(breaks = c(0, breaks), expand = c(0, 0)) +
      scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
      labs(x = "", y = expression(F(x))) +
      theme_bw()
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
      data_storage <- get(x = paste(select_data$data_type, "data_storage", sep = "_"))
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
                                 user_data_storage = user_data_storage,
                                 permanent_data_storage = permanent_data_storage,
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
