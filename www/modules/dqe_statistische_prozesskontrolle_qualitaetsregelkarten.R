dqe_statistische_prozesskontrolle_qualitaetsregelkarten_box <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_default_ui(
      id = ns("id_data_selector"),
      type = "group_dataset"
    ),
    control_chart_column_selector_ui(
      id = ns("id_control_chart_column_selector")
    )
  )
}

dqe_statistische_prozesskontrolle_qualitaetsregelkarten <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("qualitaetsregelkarten", parent, session)
  
  ns <- session$ns
  
  selected_data <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .data = .data,
    .values = .values,
    parent = self
  )
  
  selected_cols <- callModule(
    module = control_chart_column_selector,
    id = "id_control_chart_column_selector",
    .data = .data,
    .values = .values,
    parent = self,
    selected_data = selected_data
  )
}