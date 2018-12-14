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
      ),
      value = "distributions"
    ),
    tabPanel(
      title = "Zufallsstreubereiche",
      dqe_verteilungsmodelle_zufallsstreubereiche_box(
        id = ns("id_dqe_verteilungsmodelle_zufallsstreubereiche")
      ),
      value = "random_scattering_range"
    ),
    tabPanel(
      title = "Acceptance Sampling",
      dqe_verteilungsmodelle_acceptance_sampling_box(
        id = ns("id_dqe_verteilungsmodelle_acceptance_sampling")
      ),
      value = "acceptance_sampling"
    ),
    tabPanel(
      title = label_lang(
        de = "Wahrscheinlichkeitsnetze",
        en = "Probability Plotting"
      ),
      value = "probability_plotting"
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
  
  call_dqe_verteilungsmodelle_zufallsstreubereiche <- callModule(
    module = dqe_verteilungsmodelle_zufallsstreubereiche,
    id = "id_dqe_verteilungsmodelle_zufallsstreubereiche",
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
