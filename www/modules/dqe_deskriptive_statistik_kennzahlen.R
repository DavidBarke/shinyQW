dqe_deskriptive_statistik_kennzahlen_ui <- function(id) {
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

dqe_deskriptive_statistik_kennzahlen <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("kennzahlen", parent, session)
  
  ns <- session$ns
  
  .envir <- environment()
  
  rvs <- reactiveValues(
    n_table = 0,
    trigger_select_input_table_update = NULL
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
          checkboxInput(
            inputId = ns("group_by_column" %_% n_table),
            label = label_lang(
              de = "Gruppiere nach Spalten",
              en = "Group by columns"
            ),
            value = fallback(input[["group_by_column" %_% n_table]], FALSE)
          ),
          uiOutput(
            outputId = ns("data_selector_placeholder" %_% n_table)
          ),
          actionButton(
            inputId = ns("open_data" %_% n_table),
            label = label_lang(
              de = "Öffne Daten",
              en = "Open data"
            )
          )
        )
        return(ui)
      }
    })
    
    output[["data_selector_placeholder" %_% n_table]] <- renderUI({
      if (input[["group_by_column" %_% n_table]]) {
        ui <- data_selector_default_ui(
          id = ns("id_data_selector" %_% n_table),
          column = "multiple"
        )
      } else {
        ui <- data_selector_default_ui(
          id = ns("id_data_selector" %_% n_table),
          column = "no"
        )
      }
      return(ui)
    })
    
    assign(
      envir = .envir,
      "data_selector_reactive" %_% n_table,
      callModule(
        module = data_selector,
        id = "id_data_selector" %_% n_table,
        .data = .data,
        .values = .values,
        parent = self,
        unique_suffix = n_table
      )
    )
    
    observeEvent(input[["open_data" %_% n_table]], {
      .values$viewer$data$append_tab(
        tab = tabPanel(
          title = label_lang(
            de = "Statistische Kennzahlen",
            en = "Summary statistics"
          ),
          value = ns("summary_statistics" %_% n_table),
          fluidRow(
            column(
              width = 3,
              actionButton(
                inputId = ns("belonging_input_table" %_% n_table),
                label = label_lang(
                  de = "Zugehörige Input-Tabelle",
                  en = "Belonging input table"
                )
              )
            ),
            column(
              width = 3,
              actionButton(
                inputId = ns("modal_select_statistics" %_% n_table),
                label = label_lang(
                  de = "Wähle Kennzahlen",
                  en = "Select statistics"
                )
              )
            )
          ),
          dataTableOutput(
            outputId = ns("summary_statistics_datatable" %_% n_table)
          )
        ),
        select = TRUE
      )
    })
    
    observeEvent(input[["belonging_input_table" %_% n_table]], {
      # VERY UGLY, A POSSIBLE BUG IN SHINYJS DOES'NT ALLOW SELECTORS FROM NESTED
      # SESSIONS
      withReactiveDomain(
        domain = .values$session$app,
        shinyjs::show(
          selector = paste0("#element_container_tab_deskriptive_statistik_element")
        )
      )
      rvs$trigger_select_input_table_update <- list(n_table = n_table, runif(1))
    })
  })
  
  observeEvent(rvs$trigger_select_input_table_update, {
    print("Selecting")
    updateSelectInput(
      session = session,
      inputId = "select_input_table",
      selected = rvs$trigger_select_input_table_update$n_table
    )
  })
}