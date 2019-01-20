#' @export
dqe_wahrscheinlichkeitsrechnung_tabPanel <- function(id) {
  ns <- NS(id)

  list(
    tabPanel(
      title = label_lang(
        de = "Systemzuverlässigkeit",
        en = "Systemreliability"
      ),
      value = ns("system_reliability"),
      dqe_wahrscheinlichkeitsrechnung_systemzuverlaessigkeit_ui(
        id = ns("id_dqe_wahrscheinlichkeitsrechnung_systemzuverlaessigkeit")
      )
    )
  )
}

#' @export
dqe_wahrscheinlichkeitsrechnung <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("wahrscheinlichkeitsrechnung", parent, session)

  ns <- session$ns
  
  callModule(
    module = dqe_wahrscheinlichkeitsrechnung_systemzuverlaessigkeit,
    id = "id_dqe_wahrscheinlichkeitsrechnung_systemzuverlaessigkeit",
    .data = .data,
    .values = .values,
    parent = self
  )
}
