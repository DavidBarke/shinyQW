#' @export
ida_tidy_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
ida_tidy <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("tidy", parent, session)

  ns <- session$ns
}
