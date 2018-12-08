#' @export
ida_transform_dplyr_editor_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      verbatimTextOutput(
        outputId = ns("cursor_position")
      ),
      verbatimTextOutput(
        outputId = ns("code_document")
      )
    ),
    column(
      width = 8,
      aceEditor(
        outputId = ns("code"),
        value = "",
        mode = "r",
        theme = "textmate",
        tabSize = 2,
        debounce = 0,
        cursorId = ns("code_cursor"),
        selectionId = ns("code_selection"),
        # Falls autocomplete = "disabled", existiert input$code_shinyAce_hint nicht
        autoComplete = "live"
      ),
      fluidRow(
        column(
          width = 6,
          actionButton(
            inputId = ns("run_code"),
            label = "Run All"
          )
        ),
        column(
          width = 6,
          actionButton(
            inputId = ns("run_line"),
            label = "Run Zeile"
          )
        )
      ),
      verbatimTextOutput(
        outputId = ns("code_output")
      )
    )
  )
}

#' @export
ida_transform_dplyr_editor <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("dplyr_editor", parent, session)

  ns <- session$ns

  rvs <- reactiveValues(
    input = list(
      # Diese Liste wird benötigt, damit output$code_output weiß, von wem er invalidiert wird
      run_code = 0,
      run_line = 0
    ),
    # Environment, in der der Code aus dem Editor ausgewertet wird, sodass Variablen "gespeichert"
    # werden können
    # TODO: Ist das so wirklich am sinnvollsten?
    envir = environment()
  )

  output$code_output <- renderPrint({
    input <- session$input
    input$run_code
    input$run_line
    isolate({
      if (input$run_code > rvs$input$run_code) {
        rvs$input$run_code <- rvs$input$run_code + 1
        return(eval(parse(text = input$code), envir = rvs$envir))
      }
      if (input$run_line > rvs$input$run_line) {
        rvs$input$run_line <- rvs$input$run_line + 1
        return(eval(parse(text = input$code_shinyAce_hint$linebuffer), envir = rvs$envir))
      }
    })
  })

  output$cursor_position <- renderPrint({
    print(input$code_cursor)
  })

  output$code_document <- renderPrint({
    input$cursour_position
    code <- input$code_shinyAce_hint$document
    print(input$code_shinyAce_hint$document)
  })

  observeEvent(input$code, {
    print(input$code)
  })

  observeEvent(.values$einstellungen, {
    print("Observe Update")
    updateAceEditor(
      session = session,
      editorId = ns("code"),
      value = input$code,
      theme = .values$einstellungen$allgemein$ace$theme
    )
  })
}
