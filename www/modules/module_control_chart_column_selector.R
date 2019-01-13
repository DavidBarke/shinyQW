control_chart_column_selector_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$label(
      label_lang(
        de = "Zuordnung von Spalten zu Merkmalen",
        en = "Connection between columns and characteristics"
      )
    ),
    uiOutput(
      outputId = ns("select_columns")
    )
  )
}

control_chart_column_selector <- function(
  input, output, session, .data, .values, parent, selected_data, ...
) {
  self <- node$new("control_chart_column_selector", parent, session)
  
  ns <- session$ns
  
  output$select_columns <- renderUI({
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("col_numeric"),
          label = label_lang(
            de = "Stichprobenwerte",
            en = "Sample values"
          ),
          choices = names(selected_data())
        )
      ),
      column(
        width = 4,
        selectInput(
          inputId = ns("col_sample"),
          label = label_lang(
            de = "Stichprobe",
            en = "Sample"
          ),
          choices = names(selected_data())
        )
      ),
      column(
        width = 4,
        selectInput(
          inputId = ns("col_phase"),
          label = label_lang(
            de = "Phase",
            en = "Phase"
          ),
          choices = names(selected_data())
        )
      )
    )
  })
  
  selected_cols <- reactive({
    list(
      numeric = input$col_numeric,
      sample = input$col_sample,
      phase = input$col_phase
    )
  })
  
  return(selected_cols)
} 