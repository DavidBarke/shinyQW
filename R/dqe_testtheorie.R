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
dqe_testtheorie <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                            parent, ...) {
  self <- node$new("testtheorie", parent, session)

  ns <- session$ns
}
