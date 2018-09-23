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
dqe_modellstrukturen <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                 parent, ...) {
  self <- node$new("modellstrukturen", parent, session)

  ns <- session$ns

  call_dqe_modellstrukturen_lineares_modell <- callModule(module = dqe_modellstrukturen_lineares_modell,
                               id = "id_dqe_modellstrukturen_lineares_modell",
                               user_data_storage = user_data_storage,
                               permanent_data_storage = permanent_data_storage,
                               values = values,
                               parent = self)
}
