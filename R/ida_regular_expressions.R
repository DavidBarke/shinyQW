#' @export
ida_regular_expressions_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
ida_regular_expressions <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("regular_expressions", parent, session)

  ns <- session$ns
}

