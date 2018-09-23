#' @export
function_dialog_ui <- function(id) {
  ns <- NS(id)

  uiOutput(
    outputId = ns("ui_output")
  )
}

#' @export
function_dialog <- function(input, output, session,
                            user_data_storage, permanent_data_storage, values,
                            parent,
                            selected_data, rvs, ...) {
  self <- node$new("function_dialog", parent, session)

  ns <- session$ns

  call_multiple_modules_2(
    module_templates = c("select", "arrange", "filter", "mutate", "group_by", "summarise"),
    glue_module = list(x1 = "{template}", x2 = "ui"),
    glue_id = list(x1 = "id", x2 = "{template}", x3 = "ui"),
    glue_reactive = list(x1 = "call", x2 = "{template}"),
    user_data_storage = user_data_storage,
    permanent_data_storage = permanent_data_storage,
    values = values,
    session_tree = session_tree,
    selected_data = selected_data
  )

  output$ui_output <- renderUI({
    do.call(rvs$dplyr_function %_% "ui_ui", list(id = ns("id" %_% rvs$dplyr_function %_% "ui")))
  })

  output$ui_footer <- renderUI({
    do.call(rvs$dplyr_function %_% "ui_footer", list(id = ns("id" %_% rvs$dplyr_function %_% "ui")))
  })

}
