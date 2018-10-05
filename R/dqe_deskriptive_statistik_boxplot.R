#' @export
dqe_deskriptive_statistik_boxplot_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
dqe_deskriptive_statistik_boxplot <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("boxplot", parent, session)

  ns <- session$ns
}
