#' @export
dqe_deskriptive_statistik_ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    skin = "black",
    dashboardHeader(

    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Theorie",
          tabName = ns("theorie")
        ),
        menuItem(
          "Sortierte Daten",
          tabName = ns("sortierte_daten")
        ),
        menuItem(
          "Gruppierte Daten",
          tabName = ns("gruppierte_daten")
        ),
        menuItem(
          "Boxplot",
          tabName = ns("boxplot")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = ns("sortierte_daten"),
          dqe_deskriptive_statistik_sortierte_daten_ui(
            id = ns("id_dqe_deskriptive_statistik_sortierte_daten")
          )
        ),
        tabItem(
          tabName = ns("gruppierte_daten"),
          dqe_deskriptive_statistik_gruppierte_daten_ui(
            id = ns("id_dqe_deskriptive_statistik_gruppierte_daten")
          )
        ),
        tabItem(
          tabName = ns("boxplot"),
          dqe_deskriptive_statistik_boxplot_ui(
            id = ns("id_dqe_deskriptive_statistik_boxplot")
          )
        ),
        tabItem(
          tabName = ns("theorie")
          # TODO: Modul wieder reaktivieren
          # dqe_deskriptive_statistik_theorie_ui(
          #   id = ns("id_dqe_deskriptive_statistik_theorie")
          # )
        )
      )
    )
  )
}

#' @export
dqe_deskriptive_statistik <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                      parent, ...) {
  self <- node$new("deskriptive_statistik", parent, session)

  ns <- session$ns

# SORTIERTE DATEN -----------------------------------------------------------------------

  # TODO: call_multiple_modules anwenden
  call_dqe_deskriptive_statistik_sortierte_daten <- callModule(module = dqe_deskriptive_statistik_sortierte_daten,
                               id = "id_dqe_deskriptive_statistik_sortierte_daten",
                               user_data_storage = user_data_storage,
                               permanent_data_storage = permanent_data_storage,
                               values = values,
                               parent = self)
  call_dqe_deskriptive_statistik_gruppierte_daten <- callModule(module = dqe_deskriptive_statistik_gruppierte_daten,
                               id = "id_dqe_deskriptive_statistik_gruppierte_daten",
                               user_data_storage = user_data_storage,
                               permanent_data_storage = permanent_data_storage,
                               values = values,
                               parent = self)
  call_dqe_deskriptive_statistik_boxplot <- callModule(module = dqe_deskriptive_statistik_boxplot,
                               id = "id_dqe_deskriptive_statistik_boxplot",
                               user_data_storage = user_data_storage,
                               permanent_data_storage = permanent_data_storage,
                               values = values,
                               parent = self)
  # call_dqe_deskriptive_statistik_theorie <- callModule(module = dqe_deskriptive_statistik_theorie,
  #                              id = "id_dqe_deskriptive_statistik_theorie",
  #                              user_data_storage = user_data_storage,
  #                              permanent_data_storage = permanent_data_storage,
  #                              values = values,
  #                              parent = self)
}
