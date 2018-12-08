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
einstellungen_ggplot2_box <- function(id) {
  ns <- shiny::NS(id)
  tabBox(
    id = ns("tabBox_ggplot2"),
    title = "Einstellungen ggplot2",
    width = 12,
    tabPanel(
      title = "Color",
      colourInput(
        inputId = ns("col_col"),
        label = "Col",
        value = "blue"
      )
    ),
    tabPanel(
      title = "Fill",
      colourInput(
        inputId = ns("col_fill"),
        label = "Fill",
        value = "lightblue"
      )
    ),
    tabPanel(
      title = "Alpha",
      numericInput(
        inputId = ns("alpha"),
        label = "Alpha",
        value = 1,
        min = 0,
        max = 1
      )
    ),
    tabPanel(
      title = "Size",
      numericInput(
        inputId = ns("size"),
        label = "Size",
        value = 1,
        min = 0
      )
    )
  )
}

#' @export
einstellungen_ggplot2_tabPanel <- function(id) {
  ns <- NS(id)

  list(
    tabPanel(
      title = "Color",
      colourInput(
        inputId = ns("col_col"),
        label = "Col",
        value = "blue"
      )
    ),
    tabPanel(
      title = "Fill",
      colourInput(
        inputId = ns("col_fill"),
        label = "Fill",
        value = "lightblue"
      )
    ),
    tabPanel(
      title = "Alpha",
      numericInput(
        inputId = ns("alpha"),
        label = "Alpha",
        value = 1,
        min = 0,
        max = 1
      )
    ),
    tabPanel(
      title = "Size",
      numericInput(
        inputId = ns("size"),
        label = "Size",
        value = 1,
        min = 0
      )
    )
  )
}

#' @export
einstellungen_ggplot2 <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("einstellungen_ggplot2", parent, session)

  append_to_reactiveValues(
    rvs = .values$einstellungen$ggplot2,
    col = "blue",
    fill = "lightblue",
    alpha = 1,
    size = 1
  )

  ns <- session$ns

  observeEvent(input$col_col, {
    .values$einstellungen$ggplot2$col <- input$col_col
  })

  observeEvent(input$col_fill, {
    .values$einstellungen$ggplot2$fill <- input$col_fill
  })

  observeEvent(input$alpha, {
    .values$einstellungen$ggplot2$alpha <- input$alpha
  })

  observeEvent(input$size, {
    .values$einstellungen$ggplot2$size <- input$size
  })
}
