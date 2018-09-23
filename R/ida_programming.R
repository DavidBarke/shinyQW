#' @export
ida_programming_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      actionButton(
        inputId = ns("append"),
        label = "Neuer Tab"
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabset"),
        tabPanel(
          title = "A"
        )
      )
    )
  )
}

#' @export
ida_programming <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                            parent, ...) {
  self <- node$new("programming", parent, session)

  ns <- session$ns

  observeEvent(input$append, {
    appendTab(
      inputId = "tabset",
      tab = tabPanel(
        title = "B"
      )
    )
  })
}
