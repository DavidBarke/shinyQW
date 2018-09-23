#' @export
select_ui_ui <- function(id) {
  ns <- NS(id)

  list(
    uiOutput(
      outputId = ns("ui_action")
    ),
    hr(),
    uiOutput(
      outputId = ns("ui_vorschau")
    )
  )
}

select_ui_footer <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 6,
      actionButton(
        inputId = ns("submit_modal"),
        label = "Submit"
      )
    ),
    column(
      width = 6,
      actionButton(
        inputId = ns("exit_modal"),
        label = "Abbruch"
      )
    )
  )
}

#' @export
select_ui <- function(input, output, session,
                      user_data_storage, permanent_data_storage, values,
                      parent,
                      selected_data,
                      ...) {
  self <- node$new("select", parent, session)

  ns <- session$ns

  rvs <- reactiveValues(
    default = list(
      data_table = tibble("Alter Name" = character(0), "Neuer Name" = character(0))
    ),
    data_table = tibble("Alter Name" = character(0), "Neuer Name" = character(0))
  )

  data <- reactive({
    selected_data <- selected_data()$values
    data_type <- selected_data$data_type
    data_storage <- get(data_type)
    data <- data_storage[[selected_data$data$selected]]
  })

  # OUTPUT ---------------------------------------------------------------------------

  output$ui_action <- renderUI({
    ui_list <- list()

    ui_list[[1]] <- selectizeInput(
      inputId = ns("columns"),
      label = "Wähle Spaltennamen",
      multiple = TRUE,
      choices <- names(req(data()))
    )

    ui_list[[2]] <- DT::dataTableOutput(
      outputId = ns("names_columns")
    )

    return(ui_list)
  })

  output$ui_vorschau <- renderUI({
    ui_list <- list(
      h4(
        "Vorschau"
      ),
      DT::dataTableOutput(
        outputId = ns("vorschau")
      )
    )

    return(ui_list)
  })

  # Direkte Verwendung von rvs$data_table, um sicherzustellen, dass er innerhalb der render-Funktion
  # nicht verändert wird
  output$names_columns <- DT::renderDataTable(rvs$data_table, editable = TRUE)

  output$vorschau <- DT::renderDataTable(req(values$vorschau), options = list(
    # TODO: Muss das wirklich 400px sein? Kann das ein beliebiger Wert sein, vielleicht Prozent?
    scrollX = "400px"
  ))

  proxy_names_columns <- DT::dataTableProxy(
    outputId = "names_columns"
  )

  # OBSERVER --------------------------------------------------------------------

  observeEvent(input$columns, ignoreNULL = FALSE, {
    if (!all(input$columns %in% rvs$data_table[["Alter Name"]])) {
      new_rows_indices <- which(!input$columns %in% rvs$data_table[["Alter Name"]])
      new_rows <- input$columns[new_rows_indices]
      new_tibble <- tibble("Alter Name" = new_rows, "Neuer Name" = new_rows)
      rvs$data_table <- bind_rows(rvs$data_table, new_tibble)
      replaceData(proxy_names_columns, rvs$data_table)
    }
    if (!all(rvs$data_table[["Alter Name"]] %in% input$columns)) {
      old_rows_indices <- which(!rvs$data_table[["Alter Name"]] %in% input$columns)
      rvs$data_table <- rvs$data_table[-old_rows_indices, , drop = FALSE]
      replaceData(proxy_names_columns, rvs$data_table)
    }
  })

  observeEvent(input$names_columns_cell_edit, {
    cell_edit <- input$names_columns_cell_edit
    i <- cell_edit$row
    j <- cell_edit$col
    value <- cell_edit$value
    if (j == 1) {
      replaceData(proxy_names_columns, rvs$data_table)
      return()
    }
    rvs$data_table[i, j] <- DT::coerceValue(value, rvs$data_table[i, j])
    replaceData(proxy_names_columns, rvs$data_table)
  })

  # TODO: Mit nachfolgendem observeEvent funktional zusammenfassen
  # Updatet den Vorschau-Data-Table
  observeEvent({
    input$columns
    rvs$data_table
  }, {
    data <- data()
    if (!is.null(input$columns)) {
      do_selection <- function(data, ...) {
        select_vars <- quos(...)
        data <- select(data, !!!select_vars)
      }
      selection_data <- rvs$data_table[["Alter Name"]]
      names(selection_data) <- rvs$data_table[["Neuer Name"]]
      data <- do_selection(data, selection_data)
    }
    values$vorschau <- data
  })

  observeEvent(input$submit_modal, {
    # select ausführen
    if (!is.null(input$columns)) {
      data <- data()
      do_selection <- function(data, ...) {
        select_vars <- quos(...)
        data <- select(data, !!!select_vars)
      }
      selection_data <- rvs$data_table[["Alter Name"]]
      names(selection_data) <- rvs$data_table[["Neuer Name"]]
      data <- do_selection(data, selection_data)
      # rvs$data_table im data_storage überschreiben
      selected_data <- selected_data()$values
      data_type <- selected_data$data_type
      data_storage <- get(data_type)
      data_storage[[selected_data$data$selected]] <- data
      rvs$data_table <- rvs$default$data_table
    }
    removeModal()
  })

  observeEvent(input$exit_modal, {
    rvs$data_table <- rvs$default$data_table
    removeModal()
  })
}
