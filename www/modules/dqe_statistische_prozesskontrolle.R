#' @export
dqe_statistische_prozesskontrolle_tabPanel <- function(id) {
  ns <- NS(id)

  list(
    tabPanel(
      title = label_lang(
        de = "Qualitätsregelkarten",
        en = "Quality control charts"
      ),
      value = "qualitaetsregelkarten",
      dqe_statistische_prozesskontrolle_qualitaetsregelkarten_box(
        id = ns("id_dqe_statistische_prozesskontrolle_qualitaetsregelkarten")
      )
    )
  )
}

#' @export
dqe_statistische_prozesskontrolle <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("statistische_prozesskontrolle", parent, session)

  ns <- session$ns
  
  callModule(
    module = dqe_statistische_prozesskontrolle_qualitaetsregelkarten,
    id = "id_dqe_statistische_prozesskontrolle_qualitaetsregelkarten",
    .data = .data,
    .values = .values,
    parent = self
  )
}
