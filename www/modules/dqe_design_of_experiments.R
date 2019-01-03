#' Display DOE methods in a tabBox
#'
#' Shiny module ui containing methods in a \code{\link[shinydashboard]{tabBox}}.
#'
#' @param id Unique module id.
#'
#' @export
dqe_design_of_experiments_box <- function(id) {
  ns <- NS(id)

  do.call(
    collapsible_tabBox,
    c(
      list(
        id = ns("id_tabBox"),
        title = "Design of Experiments",
        width = 12
      ),
      dqe_design_of_experiments_tabPanel(
        id = id
      )
    )
  )
}

#' tabPanels containing DOE methods
#'
#' This function is especially well suited as tabPanel_list argument in
#' \code{\link[shinyQW:tabList_R6]{tabList_R6$tabBox}}.
#'
#' @param id Unique module id.
#'
#' @return A list containing \code{\link[shiny]{tabPanel}} for different methods
#' regarding Design of Experiments.
#'
#' @export
dqe_design_of_experiments_tabPanel <- function(id) {
  ns <- NS(id)

  list(
    # tabPanel(
    #   title = "Ortsauswahl",
    #   dqe_design_of_experiments_projekt_ortsauswahl_ui(
    #     id = ns("id_dqe_design_of_experiments_projekt_ortsauswahl")
    #   )
    # ),
    # tabPanel(
    #   title = "Standardisierung",
    #   dqe_design_of_experiments_projekt_standardisierung_ui(
    #     id = ns("id_dqe_design_of_experiments_projekt_standardisierung")
    #   )
    # ),
    tabPanel(
      title = label_lang(
        de = "Übersicht",
        en = "Overview"
      ),
      dqe_design_of_experiments_uebersicht_box(
        id = ns("id_dqe_design_of_experiments_uebersicht")
      )
    ),
    tabPanel(
      title = label_lang(
        de = "Versuchsplan",
        en = "Experimental design"
      ),
      dqe_design_of_experiments_projekt_versuchsplan_box(
        id = ns("id_dqe_design_of_experiments_projekt_versuchsplan")
      )
    ),
    tabPanel(
      title = "Steepest Ascent",
      dqe_design_of_experiments_projekt_steepest_ascent_ui(
        id = ns("id_dqe_design_of_experiments_projekt_steepest_ascent")
      )
    )
  )
}

#' @export
dqe_design_of_experiments <- function(input, output, session, .data, .values,
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
                            .data = .data,
                            .values = .values,
                            session_tree = session_tree,
                            parent = self)
    
    callModule(
      module = dqe_design_of_experiments_uebersicht,
      id = "id_dqe_design_of_experiments_uebersicht",
      .data = .data,
      .values = .values,
      parent = self
    )
}
