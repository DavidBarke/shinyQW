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
ida_visualize <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                          parent, ...) {
  self <- node$new("visualize", parent, session)

  ns <- session$ns
}
