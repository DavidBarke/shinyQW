#' @export
einstellungen_allgemein_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = ns("ace_theme"),
            label = "Ace Editor: Thema",
            choices = getAceThemes(),
            selected = "textmate"
          )
        ),
        column(
          width = 6
        )
      )
    ),
    column(
      width = 8
    )
  )
}

#' @export
einstellungen_allgemein <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("einstellungen_allgemein", parent, session)

  ns <- session$ns

  # Observer --------------------------------------------------------------------------

  observeEvent(input$ace_theme, {
    .values$einstellungen$allgemein$ace_theme <- input$ace_theme
  })
}
