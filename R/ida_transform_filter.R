#' @export
filter_ui_ui <- function(id) {
  ns <- NS(id)

  list(
    condition_maker_ui(
      id = ns("make_condition")
    ),
    fluidRow(
      actionButton(
        inputId = ns("add_condition"),
        label = "Füge Bedingung hinzu"
      )
    ),
    DT::dataTableOutput(
      outputId = ns("filter_conditions")
    ),
    select_data_ui(
      id = ns("id_select_data")
    )
  )

}

#' @export
filter_ui_footer <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 6,
      actionButton(
        inputId = ns("submit_modal"),
        label = "Submit"
      )
    ),
    column(
      width = 6,
      actionButton(
        inputId = ns("exit_modal"),
        label = "Abbruch"
      )
    )
  )
}

#' @export
filter_ui <- function(input, output, session,
                      user_data_storage, permanent_data_storage, values,
                      parent,
                      selected_data, ...) {
  self <- node$new("filter", parent, session)

  ns <- session$ns

  rvs <- reactiveValues(
    filter_conditions_default_table = tibble(
      Bedingung = character(0)
    )
  )

  # rvs$filter_conditions <- rvs$filter_conditions_default_table

  # OUTPUT -------------------------------------------------------------------------------

  output$filter_conditions <- DT::renderDataTable(rvs$filter_conditions, editable = TRUE)

  proxy_filter_conditions <- DT::dataTableProxy(
    outputId = "filter_conditions"
  )

  # OBSERVER -----------------------------------------------------------------------------

  observeEvent(input$filter_conditions_cell_edit, {
    cell_edit <- input$filter_conditions_cell_edit
    i <- cell_edit$row
    j <- cell_edit$col
    value <- cell_edit$value

  })

  # CALL MODULES

  call_condition_maker <- callModule(
    module = condition_maker,
    id = "make_condition",
    user_data_storage = user_data_storage,
    permanent_data_storage = permanent_data_storage,
    values = values,
    selected_data = selected_data,
    parent = self
  )

  call_select_data <- callModule(
    module = select_data,
    id = "id_select_data",
    data_rvs = list(
      user_data_storage = user_data_storage,
      permanent_data_storage = permanent_data_storage
    ),
    values,
    parent = self,
    tabset_data = tibble(
      id = c("tabset", "tabset"),
      session = c(session, session),
      type = c("append_new_tab", "append_current_tab"),
      position = c(1, 2),
      label = c("Im neuen Tab anzeigen", "Im gegenwärtigen Tab anzeigen")
    ),
    select_column = FALSE
  )

  observeEvent(input$submit_modal, {
    removeModal()
  })

  observeEvent(input$exit_modal, {
    removeModal()
  })

}
