#' @export
menubar_ui <- function(id, title) {
  ns <- NS(id)
  full_box(
    title = title,
    width = 12,
    collapsible = TRUE,
    box(title = "Box_1",
      actionButton(
        inputId = ns("append"),
        label = "Append"
      )
    ),
    box(title = "Box_2")
  )
}

#' @export
menubar <- function(
  input, output, session, data, values, parent, ...
) {
  self <- node$new("menubar", parent, session)

  observeEvent(input$append, {
    values$viewer_data$appendTab(
      tabPanel("Appended"),
      select = TRUE
    )
  })
}
