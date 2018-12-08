# Ziel des Moduls ist es, ein lineares Modell zu erstellen. Folgende Funktionalitäten
# sind vorgesehen:
# - Freie Wahl der zu betrachtenden Faktoren
# - Freie Wahl der Formel
# - Standardformeln für gängige Modelle
# - Zusammenfassung des linearen Modells
# - (Visualisierungen)

# Ein Hauptmodul, das an Userwünsche angepasst, Untermodule aufruft

#' @export
lineares_modell_ui <- function(id) {
  ns <- NS(id)

  div(
    id = ns("wrapper_lineares_modell"),
    fluidRow(
      column(
        width = 4,
        id = ns("sidebar"),
        fluidRow(
          select_data_ui(
            id = ns("id_select_data")
          )
        ),
        fluidRow(
          formula_interface_ui(
            id = ns("id_formula_interface")
          )
        ),
        fluidRow(
          column(
            width = 4,
            actionButton(
              inputId = ns("show_summary_lm_new_tab"),
              label = "Lineares Modell"
            )
          ),
          column(
            width = 4,
            actionButton(
              inputId = ns("show_summary_lm_current_tab"),
              label = "Lineares Modell"
            )
          )
        )
      ),
      column(
        width = 8,
        id = ns("main"),
        tabsetPanel(
          id = ns("tabset")
        )
      )
    )
  )
}

#' @export
lineares_modell <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("lineares_modell", parent, session)

  user_data_storage <- .data$user_data_storage
  permanent_data_storage <- .data$permanent_data_storage

  ns <- session$ns

  rvs <- reactiveValues()

  rvs$lm <- tibble(tab = numeric(0), element = numeric(0))

  linear_model <- reactive({
    input$show_summary_lm_new_tab
    input$show_summary_lm_current_tab
    isolate({
      formula <- call_formula_interface()$values$formula
      selected_data <- selected_data()
      name_data_storage <- selected_data$data_type
      data_storage <- get(name_data_storage, .data)
      data <- data_storage[[selected_data$data]]
      lm_ <- lm(formula = formula, data = data)
    })
    return(lm_)
  })

  observeEvent(input$show_summary_lm_new_tab, {
    req(linear_model())
    # Initialisierung
    if (nrow(rvs$lm) == 0) {
      tab_number <- 1
      element_number <- 1
      rvs$lm[nrow(rvs$lm) + 1,] <- list(tab_number, element_number)
    # Normaler Fall (immer das erste Element, da neuer Tab)
    } else {
      tab_number <- max(rvs$lm$tab) + 1
      element_number <- 1
      rvs$lm[nrow(rvs$lm) + 1,] <- list(tab_number, element_number)
    }
    tabId <- paste("tab_lm", tab_number, sep = "_")
    divTabId <- paste("div", tabId, sep = "_")
    divId <- paste(tabId, element_number, sep = "_")
    classOutputId <- "class_summary_lm"
    outputId <- paste("summary_lm", tab_number, element_number, sep = "_")
    divOutputId <- paste("div", outputId, sep = "_")
    closeId <- paste("close_summary_lm", tab_number, element_number, sep = "_")
    lm <- linear_model()
    ui_element <- tabPanel(
      value = ns(tabId),
      title = tabId,
      div(
        id = ns(divTabId),
        div(
          id = ns(divId),
          fluidRow(
            column(
              width = 11,
              div(
                class = ns(classOutputId),
                id = ns(divOutputId),
                verbatimTextOutput(
                  outputId = ns(outputId)
                )
              )
            ),
            column(
              width = 1,
              actionButton(
                inputId = ns(closeId),
                label = "X"
              )
            )
          )
        )
      )
    )
    observeEvent(input[[closeId]], {
      hide(
        selector = paste("#", ns(divId), sep = "")
      )
    })
    output[[outputId]] <- renderPrint({
      return(summary(isolate(linear_model())))
    })
    onevent(
      event = "dblclick",
      id = outputId,
      expr = {
        shinyjs::removeClass(
          selector = paste(".", "shiny-text-output", sep = ""),
          class = "testclass"
        )
        shinyjs::addClass(
          selector = paste("#", ns(outputId), sep = ""),
          class = "testclass"
        )
      }
    )
    appendTab(
      inputId = "tabset",
      tab = ui_element,
      select = TRUE
    )
  })

  observeEvent(input$show_summary_lm_current_tab, {
    req(linear_model())
    current_tab <- input$tabset
    # TODO: Refernez zu rvs$lm herstellen
    tab_number <- as.numeric(str_extract(current_tab, "[\\d]+$"))
    element_number <- max(filter(rvs$lm, tab == tab_number)$element) + 1
    rvs$lm[nrow(rvs$lm) + 1,] <- list(tab_number, element_number)
    tabId <- paste("tab_lm", tab_number, sep = "_")
    divTabId <- paste("div", tabId, sep = "_")
    divId <- paste(divTabId, element_number, sep = "_")
    classOutputId <- "class_summary_lm"
    outputId <- paste("summary_lm", tab_number, element_number, sep = "_")
    divOutputId <- paste("div", outputId, sep = "_")
    closeId <- paste("close_summary_lm", tab_number, element_number, sep = "_")
    lm <- linear_model()
    ui_element <- div(
      id = ns(divId),
      fluidRow(
        column(
          width = 11,
          div(
            id = ns(divOutputId),
            class = ns(classOutputId),
            verbatimTextOutput(
              outputId = ns(outputId)
            )
          )
        ),
        column(
          width = 1,
          actionButton(
            inputId = ns(closeId),
            label = "X"
          )
        )
      )
    )
    observeEvent(input[[closeId]], {
      hide(
        selector = paste("#", ns(divId), sep = "")
      )
    })
    output[[outputId]] <- renderPrint({
      return(summary(isolate(linear_model())))
    })
    onevent(
      event = "dblclick",
      id = outputId,
      expr = {
        shinyjs::removeClass(
          selector = paste(".", "shiny-text-output", sep = ""),
          class = "testclass"
        )
        shinyjs::addClass(
          selector = paste("#", ns(outputId), sep = ""),
          class = "testclass"
        )
      }
    )
    insertUI(
      selector = paste("#", ns(divTabId), sep = ""),
      where = "beforeEnd",
      ui = ui_element
    )
  })


# CALL MODULES --------------------------------------------------------------------------

  grid_select <- tibble(type = c("select", "selectize", "selectize", "selectize"),
                        position = 1:4, label = c("Zielgröße", "Erklärende Variablen", "A", "B"))

  call_select_data <- callModule(module = select_data,
                               id = "id_select_data",
                               data_rvs = .data,
                               .values = .values,
                               tabset_data = tibble(
                                 id = c("tabset", "tabset"),
                                 session = c(session, session),
                                 type = c("append_new_tab", "append_current_tab"),
                                 position = c(1, 2),
                                 label = c("Im neuen Tab anzeigen", "Im gegenwärtigen Tab anzeigen")
                               ),
                               grid_select = grid_select,
                               widgets_per_row = 3,
                               parent = self)
  call_formula_interface <- callModule(module = formula_interface,
                              id = "id_formula_interface",
                              .data = .data,
                              .values = .values,
                              parent = self,
                              data = selected_data,
                              erklaert = erklaert,
                              zielgroesse = zielgroesse)

# OBSERVE MODULES -----------------------------------------------------------------------

 selected_data <- eventReactive(call_select_data(), {
   select_data <- call_select_data()$values
   data <- list(
     data_type = select_data$data_type,
     data = select_data$data$selected
   )
   return(data)
 })

 erklaert <- eventReactive(call_select_data(), {
   return(call_select_data()$values$data$column$selected_2)
 })

 zielgroesse <- reactive({
   return(call_select_data()$values$data$column$selected_1)
 })
}

