#' @export
ida_import_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
ida_import <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                       parent, ...) {
  self <- node$new("import", parent, session)

  ns <- session$ns
}
