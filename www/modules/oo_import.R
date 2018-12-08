import_ui <- function(id, .language) {
  ns <- NS(id)

  shiny::tagList(
    import_ui_file_input(id, .language),
    import_ui_general(id, .language)
  )
}

import_ui_file_input <- function(id, .language) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fileInput(
      inputId = ns("file_input"),
      label = label_lang(
        de = "Daten-Upload",
        en = "File upload"
      ),
      buttonLabel = label_lang(
        de = "Datei auswählen",
        en = "Browse..."
      ),
      placeholder = label_lang(
        de = "Keine Datei ausgewählt",
        en = "No file selected"
      )
    )
  )
}

import_ui_preview <- function(id, .language) {
  ns <- shiny::NS(id)

  shiny::tagList(
    uiOutput(
      outputId = ns("preview_btn")
    )
  )
}

import_ui_general <- function(id, .language) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::actionButton(
      inputId = ns("import_btn"),
      label = label_lang(
        de = "Importieren",
        en = "Import"
      )
    )
  )
}

import_ui_specific <- function(id, .language) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(
      outputId = ns("specific_input")
    )
  )
}

import <- function(input, output, session, .data, .values, parent, ...) {

  self <- node$new("import", parent, session)

  ns <- session$ns
  language <- .values$.language

  rvs <- reactiveValues(
    ending_supported = FALSE,
    ending = NULL,
    data = NULL
  )

  output$preview_btn <- renderUI({
    req(rvs$ending_supported)
    shiny::actionButton(
      inputId = ns("open_preview"),
      label = label_lang(
        de = "Vorschau",
        en = "Preview"
      )
    )
  })

  observeEvent(input$file_input, {
    file_input <- input$file_input
    datapath <- file_input$datapath
    ending <- stringr::str_extract(datapath, "\\.\\w+$")
    if (ending %in% c(".csv", ".xls", ".xlsx")) {
      rvs$ending_supported <- TRUE
    } else {
      rvs$ending_supported <- FALSE
    }
    rvs$ending <- ending
  })

  observeEvent(input$open_preview, {
    .values$viewer$data$appendTab(
      shiny::tabPanel(
        title = label_lang(
          de = paste0("Vorschau: ", input$file_input$name),
          en = paste0("Preview: ", input$file_input$name)
        ),
        value = ns("preview" %_% input$file_input$name),
        DT::dataTableOutput(
          outputId = ns("preview" %_% input$file_input$name)
        )
      )
    )
    output[["preview" %_% input$file_input$name]] <- DT::renderDataTable({
      data()
    })
  })

  observeEvent(rvs$ending, {
    shiny::req(rvs$ending_supported)
    if (rvs$ending == ".csv") {
      ui <- shiny::tagList(
        shiny::radioButtons(
          inputId = ns("csv_delim"),
          label = label_lang(
            de = "Trennzeichen",
            en = "Delimiter"
          ),
          choices = c(",", ";")
        ),
        shiny::checkboxInput(
          inputId = ns("csv_col_names"),
          label = label_lang(
            de = "Spaltennamen",
            en = "Column names"
          ),
          value = TRUE
        ),
        shiny::checkboxInput(
          inputId = ns("csv_trim_ws"),
          label = label_lang(
            de = "Entferne Leerzeichen",
            en = "Trim whitespaces"
          ),
          value = TRUE
        ),
        shiny::numericInput(
          inputId = ns("csv_skip"),
          label = label_lang(
            de = "Überspringe Zeilen",
            en = "Skip rows"
          ),
          value = 0,
          min = 0
        ),
        shiny::numericInput(
          inputId = ns("csv_n_max"),
          label = label_lang(
            de = "Maximale Anzahl Zeilen",
            en = "Maximum number of rows"
          ),
          value = 1000,
          min = 1
        )
      )
    } else if (rvs$ending %in% c(".xls", ".xlsx")) {
      ui <- shiny::tagList(
        shiny::checkboxInput(
          inputId = ns("excel_col_names"),
          label = label_lang(
            de = "Spaltennamen",
            en = "Column names"
          ),
          value = TRUE
        ),
        shiny::checkboxInput(
          inputId = ns("excel_trim_ws"),
          label = label_lang(
            de = "Enferne Leerzeichen",
            en = "Trim whitespaces"
          ),
          value = TRUE
        ),
        shiny::numericInput(
          inputId = ns("excel_skip"),
          label = label_lang(
            de = "Überspringe Zeilen",
            en = "Skip rows"
          ),
          value = 0,
          min = 0
        ),
        shiny::numericInput(
          inputId = ns("excel_n_max"),
          label = label_lang(
            de = "Maximale Anzahl Zeilen",
            en = "Maximum number of rows"
          ),
          value = 1000,
          min = 1
        )
      )
    }
    output$specific_input <- renderUI({
      ui
    })
  })

  data <- reactive({
    shiny::req(rvs$ending_supported)
    data <- switch(rvs$ending,
           ".csv" = import_csv(
             datapath = req(input$file_input$datapath),
             delim = req(input$csv_delim),
             col_names = req(input$csv_col_names),
             trim_ws = req(input$csv_trim_ws),
             skip = req(input$csv_skip),
             n_max = req(input$csv_n_max)
           ),
           ".xls" = import_excel(
             datapath = req(input$file_input$datapath),
             sheet = NULL,
             range = NULL,
             col_names = req(input$excel_col_names),
             col_types = NULL,
             na = "",
             trim_ws = req(input$excel_trim_ws),
             skip = req(input$excel_skip),
             n_max = req(input$excel_n_max)
           ),
           ".xlsx" = import_excel(
             datapath = req(input$file_input$datapath),
             sheet = NULL,
             range = NULL,
             col_names = req(input$excel_col_names),
             col_types = NULL,
             na = "",
             trim_ws = req(input$excel_trim_ws),
             skip = req(input$excel_skip),
             n_max = req(input$excel_n_max)
           )
    )
    return(data)
  })

  observeEvent(input$import_btn, {

  })

}

import_csv <- function(datapath, delim, col_names, trim_ws, skip, n_max) {
  data <- readr::read_delim(
    datapath,
    delim = delim,
    col_names = col_names,
    trim_ws = trim_ws,
    skip = skip,
    n_max = n_max
  )
}

import_excel <- function(datapath, sheet, range, col_names, col_types, na,
                         trim_ws, skip, n_max) {
  readxl::read_excel(
    datapath,
    sheet = sheet,
    range = range,
    col_names = col_names,
    col_types = col_types,
    na = na,
    trim_ws = trim_ws,
    skip = skip,
    n_max = n_max
  )
}
