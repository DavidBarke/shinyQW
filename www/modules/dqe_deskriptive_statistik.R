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

#' Display descriptive statistics methods in a tabBox
#'
#' Shiny module ui containing methods in a \code{\link[shinydashboard]{tabBox}}.
#'
#' @param id Unique module id.
#'
#' @export
dqe_deskriptive_statistik_box <- function(id) {
  ns <- NS(id)

  do.call(
    collapsible_tabBox,
    c(
      list(
        id = ns("id_tabBox"),
        title = "Deskriptive Statistik",
        width = 12
      ),
      dqe_deskriptive_statistik_tabPanel(
        id = id,
        .language = .language
      )
    )
  )
}

#' tabPanels containing descriptive statistics methods
#'
#' This function is especially well suited as tabPanel_list argument in
#' \code{\link[shinyQW:tabList_R6]{tabList_R6$tabBox}}.
#'
#' @param id Unique module id.
#'
#' @return A list containing \code{\link[shiny]{tabPanel}} for different methods
#' regarding descriptive statistics.
#'
#' @export
dqe_deskriptive_statistik_tabPanel <- function(id) {
  ns <- NS(id)

  list(
    tabPanel(
      title = label_lang(
        de = "Übersicht",
        en = "Overview"
      ),
      value = "overview",
      dqe_deskriptive_statistik_uebersicht_box(
        id = ns("id_deskriptive_statistik_uebersicht")
      )
    ),
    tabPanel(
      title = label_lang(
        de = "Sortierte Daten",
        en = "Sorted data"
      ),
      value = "sorted_data",
      dqe_deskriptive_statistik_sortierte_daten_box(
        id = ns("id_dqe_deskriptive_statistik_sortierte_daten")
      )
    ),
    tabPanel(
      title = label_lang(
        de = "Gruppierte Daten",
        en = "Grouped data"
      ),
      value = "grouped_data",
      dqe_deskriptive_statistik_gruppierte_daten_box(
        id = ns("id_dqe_deskriptive_statistik_gruppierte_daten")
      )
    ),
    tabPanel(
      title = "Boxplot",
      value = "boxplot",
      dqe_deskriptive_statistik_boxplot_ui(
        id = ns("id_dqe_deskriptive_statistik_boxplot")
      )
    )
  )
}

#' @export
dqe_deskriptive_statistik <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("deskriptive_statistik", parent, session)

  ns <- session$ns

# SORTIERTE DATEN -----------------------------------------------------------------------

  # TODO: call_multiple_modules anwenden
  call_dqe_deskriptive_statistik_uebersicht <- callModule(
    module = dqe_deskriptive_statistik_uebersicht,
    id = "id_dqe_deskriptive_statistik_uebersicht",
    .data = .data,
    .values = .values,
    parent = self
  )
  call_dqe_deskriptive_statistik_sortierte_daten <- callModule(
    module = dqe_deskriptive_statistik_sortierte_daten,
    id = "id_dqe_deskriptive_statistik_sortierte_daten",
    .data = .data,
    .values = .values,
    parent = self
  )
  call_dqe_deskriptive_statistik_gruppierte_daten <- callModule(
    module = dqe_deskriptive_statistik_gruppierte_daten,
    id = "id_dqe_deskriptive_statistik_gruppierte_daten",
    .data = .data,
    .values = .values,
    parent = self
  )
  call_dqe_deskriptive_statistik_boxplot <- callModule(
    module = dqe_deskriptive_statistik_boxplot,
    id = "id_dqe_deskriptive_statistik_boxplot",
    .data = .data,
    .values = .values,
    parent = self
  )
}
