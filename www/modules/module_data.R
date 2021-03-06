#' @export
module_data_tabPanel <- function(id, .language) {
  ns <- NS(id)

  list(
    tabPanel(
      title = label_lang(
        de = "Gruppen bearbeiten",
        en = "Modify groups"
      ),
      module_data_groups_ui(
        id = ns("id_groups"),
        .language = .language
      )
    ),
    tabPanel(
      title = label_lang(
        de = "Datensätze",
        en = "Data sets"
      ),
      data_selector_extended_ui(
        id = ns("id_datasets")
      )
    ),
    tabPanel(
      title = label_lang(
        de = "Speichern und Laden",
        en = "Save and Load"
      ),
      module_data_save_load_ui(
        id = ns("id_module_data_save_load")
      )
    )
  )
}

#' @export
module_data <- function(input, output, session, .data, .values, parent, ...) {

  self <- node$new("data", parent, session)

  ns <- session$ns
  .language <- .values$.language

  callModule(
    module = module_data_groups,
    id = "id_groups",
    .data = .data,
    .values = .values,
    parent = self
  )

  callModule(
    module = data_selector,
    id = "id_datasets",
    .data = .data,
    .values = .values,
    parent = self
  )
  
  callModule(
    module = module_data_save_load,
    id = "id_module_data_save_load",
    .data = .data,
    .values = .values,
    parent = self
  )

}
