#' @export
formula_interface_ui <- function(id) {
  ns <- NS(id)

  div(
    fluidRow(
      column(
        width = 12,
        selectInput(
          inputId = ns("select_formula_type"),
          label = "Formelart",
          choices = list(
            "Einfaches lineares Modell" = list(
              "Mit Wechselwirkungen" = "interaction",
              "Ohne Wechselwirkungen" = "no_interaction"
            ),
            "Generalisiertes lineares Modell" = list(
              "Mit quadratischen Termen" = "quadratic"
            ),
            "Custom" = list(
              "Formeleditor" = "editor",
              "Formeltexter" = "text"
            )
          )
        )
      )
    ),
    fluidRow(
      id = ns("formula_input"),
      column(
        id = ns("shared_formula_input"),
        width = 6,
        div(
          id = ns("div_number_interactions"),
          numericInput(
            inputId = ns("number_interactions"),
            label = "Stufe der Wechselwirkungen",
            value = 2,
            min = 2,
            step = 1
          )
        )
      ),
      column(
        id = ns("specific_formula_input"),
        width = 6,
        div(
          id = ns("div_interaction"),
          class = ns("specific_formula_input_class")
        ),
        div(
          id = ns("div_no_interaction"),
          class = ns("specific_formula_input_class")
        ),
        div(
          id = ns("div_quadratic"),
          class = ns("specific_formula_input_class")
        ),
        div(
          id = ns("div_editor"),
          class = ns("specific_formula_input_class"),
          "Für die Zukunft geplant."
        ),
        div(
          id = ns("div_text"),
          class = ns("specific_formula_input_class"),
          textInput(
            inputId = ns("formula_text"),
            label = "Formel"
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        textOutput(
          outputId = ns("formula")
        )
      )
    )
  )
}

#' @export
formula_interface <- function(
  input, output, session, .data, .values, parent, erklaert, zielgroesse,
  ...
) {
  self <- node$new("formula_interface", parent, session)

  ns <- session$ns

  formula <- reactive({
    req(erklaert(), zielgroesse())
    if (input$number_interactions >= 2) {
      if (input$select_formula_type == "interaction") {
        formula <- paste(zielgroesse(), " ~ (", paste(erklaert(), collapse = " + "), ")^", input$number_interactions, sep = "")
      } else if (input$select_formula_type == "no_interaction") {
        formula <- paste(zielgroesse(), " ~ ", paste(erklaert(), collapse = " + "), sep = "")
      } else if (input$select_formula_type == "quadratic") {
        einfache <- paste("(", paste(erklaert(), collapse = " + "), ")", "^", input$number_interactions)
        quadratics <- paste("I(", erklaert(), "^", 2, ")", collapse = " + ", sep = "")
        formula <- paste(zielgroesse(), " ~ ", einfache, " + ", quadratics, sep = "")
      } else if (input$select_formula_type == "text") {
        formula <- input$formula_text
      }
    }
    req(formula)
    return(formula)
  })

  observeEvent(erklaert(), {
    updateNumericInput(
      session = session,
      inputId = "number_interactions",
      value = min(length(erklaert()), 3),
      max = length(erklaert())
    )
  })

  observeEvent(input$number_interactions, {
    if (input$number_interactions < 2) {
      updateNumericInput(
        session = session,
        inputId = "number_interactions",
        value = 2
      )
    }
  })

# SHOW AND HIDE ------------------------------------------------------------------------------------
  observeEvent(input$select_formula_type, {
    hide(
      selector = paste(".", ns("specific_formula_input_class"), sep = "")
    )
    show_div <- paste("div", input$select_formula_type, sep = "_")
    shinyjs::show(
      selector = paste("#", ns(show_div), sep = "")
    )
    if (input$select_formula_type %in% c("interaction", "quadratic")) {
      shinyjs::show(
        selector = paste("#", ns("div_number_interactions"), sep = "")
      )
    } else {
      shinyjs::hide(
        selector = paste("#", ns("div_number_interactions"), sep = "")
      )
    }
  })

  output$formula <- renderText({
    return(req(formula()))
  })
}
