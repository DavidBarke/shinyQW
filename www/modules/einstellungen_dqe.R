#' @export
einstellungen_dqe_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = ns("quantil_xmax"),
        label = "Default-Quantil für xmax",
        value = 0.95,
        min = 0,
        max = 1
      ),
      numericInput(
        inputId = ns("quantil_xmin"),
        label = "Default-Quantil für xmin",
        value = 0.05,
        min = 0,
        max = 1
      )
    ),
    mainPanel(

    )
  )
}

#' @export
einstellungen_dqe <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("einstellungen_dqe", parent, session)

  ns <- session$ns

  observeEvent(input$quantil_xmax, {
    delay(ms = 500, {
      if (req(input$quantil_xmax) < req(input$quantil_xmin)) {
        updateNumericInput(
          session = session,
          inputId = "quantil_xmax",
          value = input$quantil_xmin
        )
      }
    })
    .values$einstellungen$dqe$quantil_xmax <- input$quantil_xmax
  })

  observeEvent(input$quantil_xmin, {
    delay(ms = 500, {
      if (req(input$quantil_xmin) > req(input$quantil_xmax)) {
        updateNumericInput(
          session = session,
          inputId = "quantil_xmin",
          value = input$quantil_xmax
        )
      }
    })
    .values$einstellungen$dqe$quantil_xmin <- input$quantil_xmin
  })
}
