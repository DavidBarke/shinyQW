#' @export
menubar_ui <- function(id, title) {
  ns <- NS(id)
  full_box(
    title = title,
    width = 12,
    collapsible = TRUE,
    box(title = "Development",
      actionButton(
        inputId = ns("print_session_tree"),
        label = "Print Session Tree"
      )
    ),
    box(title = "Zweite Box")
  )
}

#' @export
menubar <- function(
  input, output, session, .data, .values, parent, ...
) {

  self <- node$new("menubar", parent, session)

  observeEvent(input$print_session_tree, {
    str(.values$session$tree$create_list())
  })
}
