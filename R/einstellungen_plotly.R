#' @export
einstellungen_plotly_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
einstellungen_plotly <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("einstellungen_plotly", parent, session)

  ns <- session$ns
}
