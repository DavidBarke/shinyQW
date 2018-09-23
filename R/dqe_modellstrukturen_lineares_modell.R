#' @export
dqe_modellstrukturen_lineares_modell_ui <- function(id) {
  ns <- NS(id)

  lineares_modell_ui(
    id = ns("id_lineares_modell_1")
  )
}

#' @export
dqe_modellstrukturen_lineares_modell <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                                 parent, ...) {
  self <- node$new("lineares_modell", parent, session)

  ns <- session$ns

  call_lineares_modell <- callModule(module = lineares_modell,
                               id = "id_lineares_modell_1",
                               user_data_storage = user_data_storage,
                               permanent_data_storage = permanent_data_storage,
                               values = values,
                               parent = self)
}
