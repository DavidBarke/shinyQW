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
dqe_deskriptive_statistik_boxplot <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                              parent, ...) {
  self <- node$new("boxplot", parent, session)

  ns <- session$ns
}
