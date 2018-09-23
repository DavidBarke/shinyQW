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
dqe_statistische_prozesskontrolle <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                              parent, ...) {
  self <- node$new("statistische_prozesskontrolle", parent, session)

  ns <- session$ns
}
