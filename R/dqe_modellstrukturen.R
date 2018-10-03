#' @export
dqe_modellstrukturen_ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    dashboardHeader(

    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Lineares Modell",
          tabName = ns("lineares_modell")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = ns("lineares_modell"),
          dqe_modellstrukturen_lineares_modell_ui(
            id = ns("id_dqe_modellstrukturen_lineares_modell")
          )
        )
      )
    )
  )
}

#' @export
dqe_modellstrukturen <- function(
  input, output, session, data, values, parent, ...
) {
  self <- node$new("modellstrukturen", parent, session)

  ns <- session$ns

  call_dqe_modellstrukturen_lineares_modell <- callModule(
    module = dqe_modellstrukturen_lineares_modell,
    id = "id_dqe_modellstrukturen_lineares_modell",
    data = data,
    values = values,
    parent = self
  )
}
