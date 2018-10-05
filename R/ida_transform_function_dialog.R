#' @export
function_dialog_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    uiOutput(
      outputId = ns("ui_output")
    ),
    uiOutput(
      outputId = ns("ui_footer")
    )
  )
}

#' Initialise a dialog for executing one of the dplyr verbs on a dataset
#'
#' A shiny module that calls another shiny module based on \code{dplyr_function}.
#'
#' @param parent A \code{\link{node}} object.
#' @param selected_data A \code{\link[shiny]{reactive}} for example returned by
#' \code{\link{select_data}}.
#' @param dplyr_function A \code{\link[shiny]{reactiveValues}} containing an
#' element with name 'dplyr_function' and value in \code{c("arrange", "filter",
#' "group_by", "mutate", "select", "summarise")}.
#'
#' @export
function_dialog <- function(
  input, output, session, .data, .values, parent, selected_data, dplyr_function,
  ...
) {
  self <- node$new("function_dialog", parent, session)

  ns <- session$ns

  call_multiple_modules_2(
    module_templates = c("select", "arrange", "filter", "mutate", "group_by", "summarise"),
    glue_module = list(x1 = "{template}", x2 = "ui"),
    glue_id = list(x1 = "id", x2 = "{template}", x3 = "ui"),
    glue_reactive = list(x1 = "call", x2 = "{template}"),
    .data = .data,
    .values = .values,
    selected_data = selected_data,
    parent = self
  )

  output$ui_output <- renderUI({
    do.call(dplyr_function$dplyr_function %_% "ui_ui",
            list(id = ns("id" %_% dplyr_function$dplyr_function %_% "ui")))
  })

  output$ui_footer <- renderUI({
    do.call(dplyr_function$dplyr_function %_% "ui_footer",
            list(id = ns("id" %_% rvs$dplyr_function %_% "ui")))
  })

}
