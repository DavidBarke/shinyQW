data_import_excel_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    checkboxInput(
      inputId = ns("col_names"),
      label = label_lang(
        de = "Spaltennamen vorhanden",
        en = "Column names present"
      ),
      value = TRUE
    ),
    checkboxInput(
      inputId = ns("trim_ws"),
      label = label_lang(
        de = "Enferne Leerzeichen",
        en = "Trim whitespaces"
      ),
      value = TRUE
    ),
    numericInput(
      inputId = ns("skip"),
      label = label_lang(
        de = "Überspringe Zeilen",
        en = "Skip rows"
      ),
      value = 1,
      min = 1
    ),
    numericInput(
      inputId = ns("n_max"),
      label = label_lang(
        de = "Maximale Anzahl Zeilen",
        en = "Maximum number of rows"
      ),
      value = 1000,
      min = 1
    )
  )
}

data_import_excel <- function(
  input, output, session, .data, .values, parent, datapath
) {
  
  self <- node$new("data_import_excel", parent, session)
  
  ns <- session$ns
  
  live_data <- reactive({
    read_excel(
      path = datapath(),
      sheet = NULL,
      range = NULL,
      col_names = input$col_names,
      col_types = NULL,
      na = "",
      trim_ws = input$trim_ws,
      skip = input$skip,
      n_max = input$n_max
    )
  })
  
  return(live_data)
}