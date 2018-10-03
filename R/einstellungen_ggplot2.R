#' @export
einstellungen_ggplot2_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      colourInput(
        inputId = ns("col_col"),
        label = "Col",
        value = "blue"
      ),
      colourInput(
        inputId = ns("col_fill"),
        label = "Fill",
        value = "lightblue"
      ),
      numericInput(
        inputId = ns("alpha"),
        label = "Alpha",
        value = 1,
        min = 0,
        max = 1
      ),
      numericInput(
        inputId = ns("size"),
        label = "Size",
        value = 1,
        min = 0
      )
    ),
    mainPanel(

    )
  )
}

#' @export
einstellungen_ggplot2 <- function(
  input, output, session, data, values, parent, ...
) {
  self <- node$new("einstellungen_ggplot2", parent, session)

  ns <- session$ns

  observeEvent(input$col_col, {
    rvs_values$ggplot2$col <- input$col_col
  })

  observeEvent(input$col_fill, {
    rvs_values$ggplot2$fill <- input$col_fill
  })

  observeEvent(input$alpha, {
    rvs_values$ggplot2$alpha <- input$alpha
  })

  observeEvent(input$size, {
    rvs_values$ggplot2$size <- input$size
  })

  rvs_values <- reactiveValues()

  return_user_data_storage <- reactive({
    return(NULL)
  })

  return_permanent_data_storage <- reactive({
    return(NULL)
  })

  return_values <- reactive({
    return_values <- list(
      ggplot2 = rvs_values$ggplot2
    )
    return(return_values)
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
