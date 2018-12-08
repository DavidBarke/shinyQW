#' @export
dqe_statistische_prozesskontrolle_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
dqe_statistische_prozesskontrolle <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("statistische_prozesskontrolle", parent, session)

  ns <- session$ns
}
