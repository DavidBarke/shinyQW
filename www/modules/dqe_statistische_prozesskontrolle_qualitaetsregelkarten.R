dqe_statistische_prozesskontrolle_qualitaetsregelkarten_box <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        actionButton(
          inputId = ns("add_table"),
          label = label_lang(
            de = "Neue Tabelle",
            en = "Add table"
          )
        )
      ),
      column(
        width = 6,
        uiOutput(
          outputId = ns("select_input_table")
        )
      )
    ),
    div(
      id = ns("input_tables")
    )
  )
}

dqe_statistische_prozesskontrolle_qualitaetsregelkarten <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("qualitaetsregelkarten", parent, session)
  
  ns <- session$ns
  
  .envir <- environment()
  
  rvs <- reactiveValues(
    n_table = 0
  )
  
  output$select_input_table <- renderUI({
    n_table <- rvs$n_table
    if (n_table > 0) {
      ui <- selectInput(
        inputId = ns("select_input_table"),
        label = label_lang(
          de = "Wähle Tabelle",
          en = "Select table"
        ),
        choices = label_lang_list(
          de = paste("Tabelle", seq_len(n_table), sep = " "),
          en = paste("Table", seq_len(n_table), sep = " "),
          value = seq_len(n_table)
        ),
        selected = n_table
      )
      return(ui)
    }
  })
  
  observeEvent(input$add_table, {
    rvs$n_table <- rvs$n_table + 1
    n_table <- rvs$n_table
    
    insertUI(
      selector = paste0("#", ns("input_tables")),
      where = "afterBegin",
      ui = uiOutput(
        outputId = ns("input_table" %_% n_table)
      )
    )
    
    output[["input_table" %_% n_table]] <- renderUI({
      if (input$select_input_table == n_table) {
        ui <- tagList(
          data_selector_default_ui(
            id = ns("id_data_selector" %_% n_table),
            type = "group_dataset"
          ),
          control_chart_column_selector_ui(
            id = ns("id_control_chart_column_selector" %_% n_table)
          ),
          control_chart_phase_selector_ui(
            id = ns("id_control_chart_phase_selector" %_% n_table)
          )
        )
        return(ui)
      }
    })
    
    assign(
      envir = .envir,
      "selected_data" %_% n_table,
      callModule(
        module = data_selector,
        id = "id_data_selector" %_% n_table,
        .data = .data,
        .values = .values,
        parent = self,
        unique_suffix = n_table
      )
    )
    
    assign(
      envir = .envir,
      "selected_col_names" %_% n_table,
      callModule(
        module = control_chart_column_selector,
        id = "id_control_chart_column_selector" %_% n_table,
        .data = .data,
        .values = .values,
        parent = self,
        selected_data = get("selected_data" %_% n_table),
        unique_suffix = n_table
      )
    )
    
    assign(
      envir = .envir,
      "control_chart_phases" %_% n_table,
      callModule(
        module = control_chart_phase_selector,
        id = "id_control_chart_phase_selector" %_% n_table,
        .data = .data,
        .values = .values,
        parent = self,
        phases = get("phases" %_% n_table),
        unique_suffix = n_table
      )
    )
    
    assign(
      envir = .envir,
      "phases" %_% n_table,
      reactive({
        data <- get("selected_data" %_% n_table)()
        phase_col_name <- get("selected_col_names" %_% n_table)()$phase
        unique(data[[phase_col_name]])
      })
    )
  })
}