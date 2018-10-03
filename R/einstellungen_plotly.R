#' @export
einstellungen_plotly_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(

    )
  )
}

#' @export
einstellungen_plotly <- function(
  input, output, session, data, values, parent, ...
) {
  self <- node$new("einstellungen_plotly", parent, session)

  ns <- session$ns

  return_user_data_storage <- reactive({
    return(NULL)
  })

  return_permanent_data_storage <- reactive({
    return(NULL)
  })

  return_values <- reactive({
    return(NULL)
  })

  return_list <- reactive({
    return_list <- list(
      user_data_storage = return_user_data_storage(),
      permanent_data_storage = return_permanent_data_storage(),
      values = return_values()
    )
  })

  return(return_list)
}
