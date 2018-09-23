#' @export
einstellungen_allgemein_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = ns("ace_theme"),
            label = "Ace Editor: Thema",
            choices = getAceThemes(),
            selected = "textmate"
          )
        ),
        column(
          width = 6
        )
      )
    ),
    column(
      width = 8
    )
  )
}

#' @export
einstellungen_allgemein <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                    parent, ...) {
  self <- node$new("einstellungen_allgemein", parent, session)

  ns <- session$ns

  rvs <- reactiveValues(
    allgemein = list(
      ace = list()
    )
  )

  # Observer --------------------------------------------------------------------------

  observeEvent(input$ace_theme, {
    rvs$allgemein$ace$theme <- input$ace_theme
  })

  # Return ----------------------------------------------------------------------------

  return_user_data_storage <- reactive({
    return(NULL)
  })

  return_permanent_data_storage <- reactive({
    return(NULL)
  })

  return_values <- reactive({
    return_list <- list(
      allgemein = rvs$allgemein
    )
    return(return_list)
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
