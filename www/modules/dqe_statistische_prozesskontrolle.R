#' @export
dqe_statistische_prozesskontrolle_tabPanel <- function(id) {
  ns <- NS(id)

  list(
    tabPanel(
      title = label_lang(
        de = "Zufallsstreubereiche"
      )
    ),
    tabPanel(
      title = label_lang(
        de = "Qualitätsregelkarten",
        en = "Quality control charts"
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
}
