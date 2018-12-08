#' @export
module_data_groups_ui <- function(id, .language) {
  ns <- NS(id)

  shiny::tagList(
    textInput(
      inputId = ns("new_group"),
      label = label_lang(
        de = "Name der neuen Gruppe",
        en = "Name of new group"
      )
    ),
    actionButton(
      inputId = ns("add_group"),
      label = label_lang(
        de = "Füge neue Gruppe hinzu",
        en = "Add new group"
      )
    ),
    uiOutput(
      outputId = ns("select_groups")
    ),
    actionButton(
      inputId = ns("delete_groups"),
      label = label_lang(
        de = "Entferne ausgewählte Gruppen",
        en = "Remove selected groups"
      ),
      disabled = "true"
    )
  )
}

module_data_groups <- function(input, output, session, .data, .values, parent, ...) {

  self <- node$new("data_groups", parent, session)

  ns <- session$ns
  .language <- .values$.language

  output$select_groups <- renderUI({
    selectizeInput(
      inputId = ns("select_groups"),
      label = label_lang(
        de = "Wähle eine oder mehrere Gruppen",
        en = "Select one or more groups"
      ),
      choices = .values$.data$groups,
      multiple = TRUE
    )
  })

  observeEvent(input$add_group, {
    if (!input$new_group %in% .values$.data$groups) {
      .data$add_group(input$new_group)
      .values$.data$groups <- c(.values$.data$groups, input$new_group)
      updateSelectizeInput(
        session = session,
        inputId = "select_groups",
        choices = .values$.data$groups
      )
    } else {
      updateTextInput(
        session = session,
        inputId = "new_group",
        value = label_lang(
          de = paste0(input$new_group, " existiert bereits!"),
          en = paste0(input$new_group, " exists already!")
        )
      )
    }
  })

  observeEvent(input$delete_groups, {
    showModal(modalDialog(
      title = label_lang(
        de = "Beim Entfernen von Gruppen gehen alle Daten verloren.",
        en = "All data sets in removed groups are lost."
      ),
      footer = fluidRow(
        column(
          width = 6,
          actionButton(
            inputId = ns("delete_groups_confirmed"),
            label = label_lang(
              de = "Gruppen entfernen",
              en = "Remove groups"
            )
          )
        ),
        column(
          width = 6,
          modalButton(
            label = label_lang(
              de = "Abbrechen",
              en = "Dismiss"
            )
          )
        )
      )
    ))
  })
}
