#' @export
dqe_deskriptive_statistik_boxplot_ui <- function(id) {
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

#' @export
dqe_deskriptive_statistik_boxplot <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("boxplot", parent, session)

  ns <- session$ns
  
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
          column = "single+multiple"
        )
      } else {
        ui <- data_selector_default_ui(
          id = ns("id_data_selector" %_% n_table),
          column = "single"
        )
      }
      return(ui)
    })
    
    
  })
}
