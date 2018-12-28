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

data_selector_default_ui <- function(id, type) {
  ns <- NS(id)
  
  if (type == "group_dataset") {
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
  } else if (type == "group_dataset_column") {
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
  }
}

data_selector <- function(input, output, session, .data, .values, parent, ...) {

  self <- node$new("data_selector", parent, session)

  ns <- session$ns
  .language <- .values$.language

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
    req(input$select_group)
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
    tab_values <- .values$viewer$data$get("tab_values")
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = paste0(input$select_group, ": ", input$select_dataset),
        DT::dataTableOutput(
          outputId = ns(input$select_group %_% input$select_dataset)
        ),
        value = ns(input$select_group %_% input$select_dataset)
      )
    )
    output[[input$select_group %_% input$select_dataset]] <- DT::renderDataTable({
      isolate(
        DT::datatable(
          .data$get_dataset(input$select_group, input$select_dataset),
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = c("copy", "csv", "excel", "pdf", "print")
          )
        )
      )
    })
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
    if (is.null(input$select_column)) {
      return(.data$get_dataset(input$select_group, input$select_dataset))
    } else {
      return(.data$get_column(input$select_group, input$select_dataset, input$select_column))
    }
  })
  
  return(selected)
}
