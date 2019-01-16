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
  input, output, session, .data, .values, parent, selected_data, 
  unique_suffix = NULL, ...
) {
  self <- node$new(
    paste0("control_chart_column_selector", unique_suffix), 
    parent, 
    session
  )
  
  ns <- session$ns
  
  output$select_columns <- renderUI({
    choices = names(selected_data())
    fluidRow(
      column(
        width = 4,
        selectInput(
          inputId = ns("col_numeric"),
          label = label_lang(
            de = "Stichprobenwerte",
            en = "Sample values"
          ),
          choices = choices,
          selected = fallback(input$col_numeric, choices[1])
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
          choices = choices,
          selected = fallback(input$col_sample, choices[1])
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
          choices = choices,
          selected = fallback(input$col_phase, choices[1])
        )
      )
    )
  })
  
  selected_cols <- reactive({
    list(
      value = input$col_numeric,
      sample = input$col_sample,
      phase = input$col_phase
    )
  })
  
  return(selected_cols)
} 