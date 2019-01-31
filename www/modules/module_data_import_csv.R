data_import_csv_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    radioButtons(
      inputId = ns("delim"),
      label = label_lang(
        de = "Trennzeichen",
        en = "Delimiter"
      ),
      choices = c(",", ";")
    ),
    checkboxInput(
      inputId = ns("col_names"),
      label = label_lang(
        de = "Spaltennamen",
        en = "Column names"
      ),
      value = TRUE
    ),
    checkboxInput(
      inputId = ns("trim_ws"),
      label = label_lang(
        de = "Entferne Leerzeichen",
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
      value = 0,
      min = 0
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

data_import_csv <- function(
  input, output, session, .data, .values, parent, datapath
) {
  
  self <- node$new("data_import_csv", parent, session)
  
  ns <- session$ns
  
  rvs <- reactiveValues(
    error = FALSE
  )
  
  live_data <- reactive({
    data <- read_delim(
      datapath(),
      delim = input$delim,
      col_names = input$col_names,
      trim_ws = input$trim_ws,
      skip = input$skip,
      n_max = input$n_max
    )
  })
  
  error <- reactive({
    rvs$error
  })
  
  return(list(
    data = live_data,
    error = error
  ))
}