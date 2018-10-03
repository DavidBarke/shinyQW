#' @export
dqe_design_of_experiments_ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    skin = "black",
    dashboardHeader(

    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Projekt",
          menuSubItem(
            "Ortsauswahl",
            tabName = ns("ortsauswahl")
          ),
          menuSubItem(
            "Standardisierung",
            tabName = ns("standardisierung")
          ),
          menuSubItem(
            "Versuchsplan",
            tabName = ns("versuchsplan")
          ),
          menuSubItem(
            "Steepest Ascent",
            tabName = ns("steepest_ascent")
          )
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = ns("ortsauswahl"),
          dqe_design_of_experiments_projekt_ortsauswahl_ui(
            id = ns("id_dqe_design_of_experiments_projekt_ortsauswahl")
          )
        ),
        tabItem(
          tabName = ns("standardisierung"),
          dqe_design_of_experiments_projekt_standardisierung_ui(
            id = ns("id_dqe_design_of_experiments_projekt_standardisierung")
          )
        ),
        tabItem(
          tabName = ns("versuchsplan"),
          dqe_design_of_experiments_projekt_versuchsplan_ui(
            id = ns("id_dqe_design_of_experiments_projekt_versuchsplan")
          )
        ),
        tabItem(
          tabName = ns("steepest_ascent"),
          dqe_design_of_experiments_projekt_steepest_ascent_ui(
            id = ns("id_dqe_design_of_experiments_projekt_steepest_ascent")
          )
        )
      )
    )
  )
}

#' @export
dqe_design_of_experiments <- function(input, output, session, data, values,
                                      parent, ...) {

  self <- node$new("design_of_experiments", parent, session)

  ns <- session$ns

  rvs <- reactiveValues()

# CALL MODULES --------------------------------------------------------------------------

  themen <- c("ortsauswahl", "standardisierung", "versuchsplan", "steepest_ascent")

    call_multiple_modules_2(module_templates = themen,
                            glue_module = list(x1 = "{prefix}", x2 = "{template}"),
                            glue_id = list(x1 = "id", x2 = "{prefix}", x3 = "{template}"),
                            glue_reactive = list(x1 = "call", x2 = "{prefix}", x3 = "{template}"),
                            glue_list = list(prefix = "dqe_design_of_experiments_projekt"),
                            data = data,
                            values = values,
                            session_tree = session_tree,
                            parent = self)
}
