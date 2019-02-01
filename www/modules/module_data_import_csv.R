data_import_csv_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 11,
      uiOutput(
        outputId = ns("module_ui")
      )
    ),
    column(
      width = 1,
      actionButton(
        inputId = ns("modal_default_settings"),
        label = NULL,
        icon = icon("cog")
      )
    )
  )
}

data_import_csv <- function(
  input, output, session, .data, .values, parent, datapath
) {
  
  self <- node$new("data_import_csv", parent, session)
  
  ns <- session$ns
  
  rvs <- reactiveValues(
    error = FALSE,
    default_settings = list(
      delim = ",",
      col_names = TRUE,
      trim_ws = TRUE,
      skip = 0,
      n_max = 1000
    )
  )
  
  output$module_ui <- renderUI({
    tagList(
      radioButtons(
        inputId = ns("delim"),
        label = label_lang(
          de = "Trennzeichen",
          en = "Delimiter"
        ),
        choices = c(",", ";"),
        selected = rvs$default_settings$delim
      ),
      checkboxInput(
        inputId = ns("col_names"),
        label = label_lang(
          de = "Spaltennamen",
          en = "Column names"
        ),
        value = rvs$default_settings$col_names
      ),
      checkboxInput(
        inputId = ns("trim_ws"),
        label = label_lang(
          de = "Entferne Leerzeichen",
          en = "Trim whitespaces"
        ),
        value = rvs$default_settings$trim_ws
      ),
      numericInput(
        inputId = ns("skip"),
        label = label_lang(
          de = "Überspringe Zeilen",
          en = "Skip rows"
        ),
        value = rvs$default_settings$skip,
        min = 0
      ),
      numericInput(
        inputId = ns("n_max"),
        label = label_lang(
          de = "Maximale Anzahl Zeilen",
          en = "Maximum number of rows"
        ),
        value = rvs$default_settings$n_max,
        min = 1
      )
    )
  })
  
  observeEvent(input$modal_default_settings, {
    showModal(modalDialog(
      title = label_lang(
        de = "Standardeinstellungen",
        en = "Default settings"
      ),
      easyClose = TRUE,
      radioButtons(
        inputId = ns("default_delim"),
        label = label_lang(
          de = "Trennzeichen",
          en = "Delimiter"
        ),
        choices = c(",", ";"),
        selected = rvs$default_settings$delim
      ),
      checkboxInput(
        inputId = ns("default_col_names"),
        label = label_lang(
          de = "Spaltennamen",
          en = "Column names"
        ),
        value = rvs$default_settings$col_names
      ),
      checkboxInput(
        inputId = ns("default_trim_ws"),
        label = label_lang(
          de = "Entferne Leerzeichen",
          en = "Trim whitespaces"
        ),
        value = rvs$default_settings$trim_ws
      ),
      numericInput(
        inputId = ns("default_skip"),
        label = label_lang(
          de = "Überspringe Zeilen",
          en = "Skip rows"
        ),
        value = rvs$default_settings$skip,
        min = 0
      ),
      numericInput(
        inputId = ns("default_n_max"),
        label = label_lang(
          de = "Maximale Anzahl Zeilen",
          en = "Maximum number of rows"
        ),
        value = rvs$default_settings$n_max,
        min = 1
      ),
      footer = actionButton(
        inputId = ns("confirm_default_settings"),
        label = label_lang(
          de = "Bestätigen",
          en = "Confirm"
        )
      )
    ))
  })
  
  observeEvent(input$confirm_default_settings, {
    settings <- names(rvs$default_settings)
    for (setting in settings) {
      rvs$default_settings[[setting]] <- input[["default" %_% setting]]
    }
    removeModal()
  })
  
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