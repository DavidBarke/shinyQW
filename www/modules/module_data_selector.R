#' @export
data_selector_extended_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 6,
        uiOutput(
          outputId = ns("select_group")
        )
      ),
      column(
        width = 6,
        uiOutput(
          outputId = ns("select_dataset")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        uiOutput(
          outputId = ns("view_dataset")
        )
      ),
      column(
        width = 6,
        uiOutput(
          outputId = ns("switch_group")
        )
      )
    )
  )
}

data_selector_default_ui <- function(
  id, column = c("no", "single", "multiple")
) {
  ns <- NS(id)
  
  column <- match.arg(column)
  
  if (column == "no") {
    ui <- tagList(
      fluidRow(
        column(
          width = 6,
          uiOutput(
            outputId = ns("select_group")
          )
        ),
        column(
          width = 6,
          uiOutput(
            outputId = ns("select_dataset")
          )
        )
      )
    )
  } else if (column == "single") {
    ui <- fluidRow(
      column(
        width = 4,
        uiOutput(
          outputId = ns("select_group")
        )
      ),
      column(
        width = 4,
        uiOutput(
          outputId = ns("select_dataset")
        )
      ),
      column(
        width = 4,
        uiOutput(
          outputId = ns("select_column")
        )
      )
    )
  } else if (column == "multiple") {
    ui <- fluidRow(
      column(
        width = 4,
        uiOutput(
          outputId = ns("select_group")
        )
      ),
      column(
        width = 4,
        uiOutput(
          outputId = ns("select_dataset")
        )
      ),
      column(
        width = 4,
        uiOutput(
          outputId = ns("select_columns")
        )
      )
    )
  }
  ui
}

data_selector <- function(
  input, output, session, .data, .values, parent, unique_suffix = NULL, ...
) {
  
  self <- node$new(paste0("data_selector", unique_suffix), parent, session)

  ns <- session$ns
  
  rvs <- reactiveValues(
    view_dataset_counter = 0
  )

  output$select_group <- renderUI({
    choices = .values$.data$groups
    selectInput(
      inputId = ns("select_group"),
      label = label_lang(
        de = "Wähle Gruppe",
        en = "Select group"
      ),
      choices = choices,
      selected = fallback(input$select_group, choices[1])
    )
  })

  output$select_dataset <- renderUI({
    # req(input$select_group)
    choices = .data$get_datasets_names(input$select_group)
    req(length(choices) != 0)
    selectInput(
      inputId = ns("select_dataset"),
      label = label_lang(
        de = "Wähle Datensatz",
        en = "Select dataset"
      ),
      choices = choices,
      selected = fallback(input$select_dataset, choices[1])
    )
  })
  
  output$select_column <- renderUI({
    req(input$select_group, input$select_dataset)
    choices = .data$get_dataset_columns(input$select_group, input$select_dataset)
    req(length(choices) != 0)
    selectInput(
      inputId = ns("select_column"),
      label = label_lang(
        de = "Wähle Spalte",
        en = "Select column"
      ),
      choices = choices,
      selected = fallback(input$select_column, choices[1])
    )
  })
  
  output$select_columns <- renderUI({
    req(input$select_group, input$select_dataset)
    choices = .data$get_dataset_columns(input$select_group, input$select_dataset)
    req(length(choices) != 0)
    selectInput(
      inputId = ns("select_columns"),
      label = label_lang(
        de = "Wähle Spalten",
        en = "Select columns"
      ),
      choices = choices,
      selected = fallback(input$select_columns, choices[1]),
      multiple = TRUE
    )
  })

  output$view_dataset <- renderUI({
    req(input$select_group)
    choices = .data$get_datasets_names(input$select_group)
    req(length(choices) != 0)
    actionButton(
      inputId = ns("view_dataset"),
      label = label_lang(
        de = "Betrachte Datensatz",
        en = "View dataset"
      )
    )
  })

  observeEvent(input$view_dataset, {
    rvs$view_dataset_counter <- rvs$view_dataset_counter + 1
    view_dataset_counter <- rvs$view_dataset_counter
    selected_group <- input$select_group
    selected_dataset <- input$select_dataset
    tab_values <- .values$viewer$data$get("tab_values")
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = paste0(selected_group, ": ", selected_dataset),
        fluidRow(
          column(
            width = 3,
            downloadButton(
              outputId = ns("csv_download" %_% view_dataset_counter),
              label = "CSV"
            )
          ),
          column(
            width = 3,
            downloadButton(
              outputId = ns("xlsx_download" %_% view_dataset_counter),
              label = "Excel-CSV"
            )
          )
        ),
        DT::dataTableOutput(
          outputId = ns("view_dataset" %_% view_dataset_counter)
        ),
        value = ns(selected_group %_% selected_dataset)
      )
    )
    
    output[["view_dataset" %_% view_dataset_counter]] <- DT::renderDataTable({
      # Verweis auf selected_group und selected_dataset anstelle der Inputs, um
      # die Reaktivität aufzulösen
      datatable(.data$get_dataset(selected_group, selected_dataset))
    })
    
    root_filepath <- selected_group %_% selected_dataset %_% Sys.Date()
    
    output[["csv_download" %_% view_dataset_counter]] <- downloadHandler(
      filename = function() {
        paste0(root_filepath, ".csv")
      },
      content = function(file) {
        write_csv(.data$get_dataset(selected_group, selected_dataset), file)
      },
      contentType = "text/csv"
    )
    
    output[["xlsx_download" %_% view_dataset_counter]] <- downloadHandler(
      filename = function() {
        paste0(root_filepath, ".csv")
      },
      content = function(file) {
        write_excel_csv(.data$get_dataset(selected_group, selected_dataset), file)
      },
      contentType = "text/csv"
    )
  })


  output$switch_group <- renderUI({
    req(input$select_group)
    choices = .data$get_datasets_names(input$select_group)
    req(length(choices) != 0)
    actionButton(
      inputId = ns("switch_group"),
      label = label_lang(
        de = "Ändere Gruppe",
        en = "Modify group"
      )
    )
  })

  observeEvent(input$switch_group, {
    showModal(
      shiny::modalDialog(
        title = label_lang(
          de = "Ändere Gruppe",
          en = "Modify group"
        ),
        easyClose = TRUE,
        selectInput(
          inputId = ns("select_new_group"),
          label = label_lang(
            de = "Wähle Gruppe",
            en = "Select group"
          ),
          choices = setdiff(.values$.data$groups, input$select_group)
        ),
        tags$span(
          actionButton(
            inputId = ns("transfer_new_group"),
            label = label_lang(
              de = "Wechsle Gruppe",
              en = "Switch to group"
            )
          ),
          actionButton(
            inputId = ns("add_new_group"),
            label = label_lang(
              de = "Füge zu Gruppe hinzu",
              en = "Add to group"
            )
          )
        )
      )
    )
  })

  observeEvent(input$transfer_new_group, {
    .data$switch_group(
      name = input$select_dataset,
      group_old = input$select_group,
      group_new = input$select_new_group
    )
    updateSelectInput(
      session = session,
      inputId = "select_dataset",
      choices = .data$get_datasets_names(input$select_group)
    )
  })

  observeEvent(input$add_new_group, {
    .data$switch_group(
      name = input$select_dataset,
      group_old = input$select_group,
      group_new = input$select_new_group,
      remove = FALSE
    )
    updateSelectInput(
      session = session,
      inputId = "select_dataset",
      choices = .data$get_datasets_names(input$select_group)
    )
  })
  
  selected <- reactive({
    req(input$select_group, input$select_dataset)
    list(
      group = input$select_group,
      data_name = input$select_dataset,
      data_val = .data$get_dataset(input$select_group, input$select_dataset),
      col_name = input$select_column,
      col_val = .data$get_column(
        input$select_group, input$select_dataset, input$select_column
      ),
      col_names = input$select_columns
    )
  })
  
  return(selected)
}
