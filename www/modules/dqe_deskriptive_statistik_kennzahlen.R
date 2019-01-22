dqe_deskriptive_statistik_kennzahlen_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxInput(
      inputId = ns("group_by_column"),
      label = label_lang(
        de = "Gruppiere basierend auf einer Spalte",
        en = "Group based on column"
      )
    ),
    uiOutput(
      outputId = ns("data_selector_placeholder")
    )
  )
}

dqe_deskriptive_statistik_kennzahlen <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("kennzahlen", parent, session)
  
  ns <- session$ns
  
  output$data_selector_placeholder <- renderUI({
    if (input$group_by_column) {
      ui <- data_selector_default_ui(
        id = ns("id_data_selector"),
        column = "multiple"
      )
    } else {
      ui <- data_selector_default_ui(
        id = ns("id_data_selector"),
        column = "no"
      )
    }
    return(ui)
  })
  
  data_selector_reactive <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .data = .data,
    .values = .values,
    parent = self
  )
}