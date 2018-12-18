dqe_verteilungsmodelle_zufallsstreubereiche_box <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 3,
        radioButtons(
          inputId = ns("nseitig"),
          label = label_lang(
            de = "Art",
            en = "Type"
          ),
          choices = label_lang_list(
            de = c("Einseitig", "Zweiseitig"),
            en = c("One-sided", "Two-sided"),
            value = c("one_sided", "two_sided")
          ),
          inline = FALSE
        )
      ),
      column(
        width = 3,
        numericInput(
          inputId = ns("alpha"),
          label = label_lang(
            de = "Signifikanzniveau",
            en = "Level of significance"
          ),
          value = 0.05,
          min = 0,
          max = 1
        )
      ),
      column(
        width = 6,
        module_verteilungen_input_header(
          id = ns("id_module_verteilungen_input")
        )
      )
    ),
    module_verteilungen_input_tables(
      id = ns("id_module_verteilungen_input")
    )
  )
}

dqe_verteilungsmodelle_zufallsstreubereiche <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("zufallsstreubereiche", parent, session)
  
  ns <- session$ns
  
  call_module_verteilungen_input <- callModule(
    module = module_verteilungen_input,
    id = "id_module_verteilungen_input",
    .data = .data,
    .values = .values,
    parent = self
  )
}