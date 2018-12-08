#' @export
dqe_verteilungsmodelle_tabPanel <- function(id) {
  ns <- NS(id)

  list(
    tabPanel(
      title = label_lang(
        de = "Verteilungen",
        en = "Distributions"
      ),
      dqe_verteilungsmodelle_verteilungen_box(
        id = ns("id_dqe_verteilungsmodelle_verteilungen")
      )
    ),
    tabPanel(
      title = "Acceptance Sampling",
      dqe_verteilungsmodelle_acceptance_sampling_box(
        id = ns("id_dqe_verteilungsmodelle_acceptance_sampling")
      )
    ),
    tabPanel(
      title = label_lang(
        de = "Wahrscheinlichkeitsnetze",
        en = "Probability Plotting"
      )
    )
  )
}

#' @export
dqe_verteilungsmodelle <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("verteilungsmodelle", parent, session)

  ns <- session$ns

  call_dqe_verteilungsmodelle_verteilungen <- callModule(
    module = dqe_verteilungsmodelle_verteilungen,
    id = "id_dqe_verteilungsmodelle_verteilungen",
    .data = .data,
    .values = .values,
    parent = self
  )

  call_dqe_verteilungsmodelle_acceptance_sampling <- callModule(
    module = dqe_verteilungsmodelle_acceptance_sampling,
    id = "id_dqe_verteilungsmodelle_acceptance_sampling",
    .data = .data,
    .values = .values,
    parent = self
  )
}
