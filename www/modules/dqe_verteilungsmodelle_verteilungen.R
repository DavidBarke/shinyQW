dqe_verteilungsmodelle_verteilungen_box <- function(id) {
  ns <- NS(id)

  tagList(
    module_verteilungen_input_header(
      id = ns("id_module_verteilungen_input")
    ),
    module_verteilungen_input_tables(
      id = ns("id_module_verteilungen_input")
    )
  )
}

dqe_verteilungsmodelle_verteilungen <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("verteilungen", parent, session)

  ns <- session$ns

  call_module_verteilungen_input <- callModule(
    module = module_verteilungen_input,
    id = "id_module_verteilungen_input",
    .data = .data,
    .values = .values,
    parent = self,
    .mode = "standard"
  )
}
