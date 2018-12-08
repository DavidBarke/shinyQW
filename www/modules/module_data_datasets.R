#' @export
module_data_datasets_ui <- function(id, .language) {
  ns <- NS(id)

  shiny::tagList(
    uiOutput(
      outputId = ns("select_group")
    ),
    uiOutput(
      outputId = ns("select_dataset")
    ),
    tags$span(
      uiOutput(
        outputId = ns("view_dataset")
      ),
      uiOutput(
        outputId = ns("switch_group")
      )
    )
  )
}

module_data_datasets <- function(input, output, session, .data, .values, parent, ...) {

  self <- node$new("data_datasets", parent, session)

  ns <- session$ns
  .language <- .values$.language

  output$select_group <- renderUI({
    selectInput(
      inputId = ns("select_group"),
      label = label_lang(
        de = "Wähle Gruppe",
        en = "Select group"
      ),
      choices = .values$.data$groups
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
      choices = choices
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
}
