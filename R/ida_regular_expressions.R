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
ida_regular_expressions <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                    parent, ...) {
  self <- node$new("regular_expressions", parent, session)

  ns <- session$ns
}

