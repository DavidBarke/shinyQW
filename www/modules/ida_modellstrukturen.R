#' @export
ida_modellstrukturen_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
ida_modellstrukturen <- function(
  input, output, session, data, values,
  parent, ...
) {
  self <- node$new("modellstrukturen", parent, session)

  ns <- session$ns
}
