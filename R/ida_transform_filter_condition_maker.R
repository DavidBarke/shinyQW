condition_modes <- function(input, side, rvs) {
  # Verwendung von substitute auf Notwendigkeit getestet
  substitute(
    {
      print("Working substitution")
      switch(input[["mode" %_% side]],
             column = selectInput(
               inputId = ns("column" %_% side),
               label = "Spalte",
               choices = rvs$col_names
             ),
             numeric = numericInput(
               inputId = ns("numeric" %_% side),
               label = "Wert",
               value = 0
             ),
             character = selectizeInput(
               inputId = ns("character" %_% side),
               label = "Wert (Liste)",
               choices = NULL,
               multiple = TRUE,
               options = list(
                 create = TRUE
               )
             )
      )
    }
    , list(
      side = side
    )
  )
}

#' @export
condition_maker_ui <- function(id) {
  ns <- NS(id)

  list(
    fluidRow(
      column(
        width = 5,
        selectInput(
          inputId = ns("mode_lhs"),
          label = "Typ der linken Seite",
          choices = c(Spalte = "column", Zahlwert = "numeric", Text = "character")
        )
      ),
      column(
        width = 2
      ),
      column(
        width = 5,
        selectInput(
          inputId = ns("mode_rhs"),
          label = "Typ der rechten Seite",
          choices = c(Spalte = "column", Zahlwert = "numeric", Text = "character")
        )
      )
    ),
    fluidRow(
      column(
        width = 5,
        uiOutput(
          outputId = ns("lhs")
        )
      ),
      column(
        width = 2,
        selectInput(
          inputId = ns("operator"),
          label = "Operator",
          choices = c("==", ">=", "<=", ">", "<", "%in%")
        )
      ),
      column(
        width = 5,
        uiOutput(
          outputId = ns("rhs")
        )
      )
    )
  )
}

#' @export
condition_maker <- function(
  input, output, session, .data, .values, selected_data, parent, ...
) {
  self <- node$new("condition_maker", parent, session)

  ns <- session$ns

  rvs <- reactiveValues(col_names = "hans")

  output$lhs <- renderUI(condition_modes(input, "lhs", rvs), quoted = TRUE)

  output$rhs <- renderUI(condition_modes(input, "rhs", rvs), quoted = TRUE)

  observe({
    rvs$col_names <- names(data())
  })

  data <- reactive({
    selected_data <- selected_data()$values
    data_type <- selected_data$data_type
    data_storage <- get(data_type, .data)
    data <- data_storage[[selected_data$data$selected]]
  })

}
