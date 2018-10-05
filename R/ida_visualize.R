#' @export
ida_visualize_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
ida_visualize <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("visualize", parent, session)

  ns <- session$ns
}
