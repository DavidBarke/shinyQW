#' @export
dqe_testtheorie_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
dqe_testtheorie <- function(
  input, output, session, data, values, parent, ...
) {
  self <- node$new("testtheorie", parent, session)

  ns <- session$ns
}
