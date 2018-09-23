#' @export
dqe_wahrscheinlichkeitsrechnung_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
dqe_wahrscheinlichkeitsrechnung <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                            parent, ...) {
  self <- node$new("wahrscheinlichkeitsrechnung", parent, session)

  ns <- session$ns
}
