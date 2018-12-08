#' @export
dqe_modellstrukturen_lineares_modell_ui <- function(id) {
  ns <- NS(id)

  lineares_modell_ui(
    id = ns("id_lineares_modell_1")
  )
}

#' @export
dqe_modellstrukturen_lineares_modell <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("lineares_modell", parent, session)

  ns <- session$ns

  call_lineares_modell <- callModule(module = lineares_modell,
                               id = "id_lineares_modell_1",
                               .data = .data,
                               .values = .values,
                               parent = self)
}
