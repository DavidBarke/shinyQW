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
  
  statistics_choices <- label_lang_list(
    de = c(
      "Mittelwert", "Median", "Standardabweichung", "Varianz", "IQR",
      "Minimum", "Maximum", "Spannweite"
    ),
    en = c(
      "Mean", "Median", "Standard deviation", "Variance", "IQR",
      "Minmum", "Maximum", "Range"
    ),
    value = c(
      "mean", "median", "sd", "var", "IQR", "min", "max", "range"
    )
  )
  
  to_lang_staticstic_choices <- label_lang_convert_fun(
    value = c("mean", "median", "sd", "var", "IQR", "min", "max", "range"),
    de = c("Mittelwert", "Median", "Standardabweichung", "Varianz", "IQR", 
           "Minimum", "Maximum", "Spannweite"),
    en = c("Mean", "Median", "Standard deviation", "Variance", "IQR",
           "Minimum", "Maximum", "Spannweite")
  )
  
  statistics_choices_fun <- list(
    mean = mean,
    median = median,
    sd = sd,
    var = var,
    IQR = IQR,
    min = min,
    max = max,
    range = function(x) {max(x) - min(x)}
  )
  
  rvs <- reactiveValues(
    n_table = 0,
    selected_statistics = list(),
    transpose_datatable = logical(),
    remove_na = logical()
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
    rvs$selected_statistics[[n_table]] <- as.character(statistics_choices)
    rvs$transpose_datatable[n_table] <- FALSE
    rvs$remove_na[n_table] <- TRUE
    
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
    
    assign(
      envir = .envir,
      "data_selector_return" %_% n_table,
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
          actionButton(
            inputId = ns("belonging_input_table" %_% n_table),
            label = label_lang(
              de = "Zugehörige Input-Tabelle",
              en = "Belonging input table"
            )
          ),
          actionButton(
            inputId = ns("modal_settings" %_% n_table),
            label = label_lang(
              de = "Einstellungen",
              en = "Settings"
            )
          ),
          dataTableOutput(
            outputId = ns("summary_statistics_datatable" %_% n_table)
          )
        ),
        select = TRUE
      )
    })
    
    output[["summary_statistics_datatable" %_% n_table]] <- renderDataTable({
      data_selector_return <- get("data_selector_return" %_% n_table)
      data <- data_selector_return$data_val()
      summary_column <- data_selector_return$col_name()
      # Handles switching of dataset
      req(summary_column %in% names(data))
      
      # group_by_columns needs to be predefined for the gathering
      group_by_columns <- character()
      if (input[["group_by_column" %_% n_table]]) {
        group_by_columns <- data_selector_return$col_names()
        # Handles switching of dataset
        req(group_by_columns %in% names(data))
        data <- group_by_at(data, group_by_columns)
      }
      
      selected_statistics <- rvs$selected_statistics[[n_table]]
      
      q <- list()
      
      for (stat in selected_statistics) {
        fun <- statistics_choices_fun[[stat]]
        name <- to_lang_staticstic_choices(stat)
        q[[name]] <- quo((!!fun)(!!sym(summary_column)))
      }
      
      data <- summarise(data, !!!q)
      
      key <- label_lang(
        de = "Statistik",
        en = "Statistic"
      )
      
      value <- label_lang(
        de = "Wert",
        en = "Value"
      )
      
      if (!rvs$transpose_datatable[n_table]) {
        data <- gather(
          data,
          key = !!key,
          value = !!value,
          -!!group_by_columns
        )
      }
      
      data <- datatable(data)
      data <- formatRound(data, seq_along(data))
    })
    
    observeEvent(input[["belonging_input_table" %_% n_table]], {
      .values$viewer$content$add_element_by_id(
        "tab_deskriptive_statistik_element"
      )
      updateSelectInput(
        session = session,
        inputId = "select_input_table",
        selected = n_table
      )
    })
    
    observeEvent(input[["modal_settings" %_% n_table]], {
      showModal(modalDialog(
        title = label_lang(
          de = "Einstellungen",
          en = "Settings"
        ),
        easyClose = TRUE,
        checkboxGroupInput(
          inputId = ns("select_statistics" %_% n_table),
          label = NULL,
          choices = as.character(statistics_choices),
          selected = rvs$selected_statistics[[n_table]]
        ),
        checkboxInput(
          inputId = ns("transpose_datatable" %_% n_table),
          label = label_lang(
            de = "Transponiere Tabelle",
            en = "Transpose table"
          ),
          value = fallback(rvs$transpose_datatable[n_table], FALSE)
        ),
        checkboxInput(
          inputId = ns("remove_na" %_% n_table),
          label = label_lang(
            de = "Gegenwärtig nicht funktional",
            en = "Currently defunct"
          ),
          value = fallback(rvs$remove_na[n_table], TRUE)
        ),
        footer = tagList(
          actionButton(
            inputId = ns("apply_all_select_statistics" %_% n_table),
            label = label_lang(
              de = "Auf alle anwenden",
              en = "Apply "
            )
          ),
          actionButton(
            inputId = ns("apply_select_statistics" %_% n_table),
            label = label_lang(
              de = "Anwenden",
              en = "Apply"
            )
          )
        )
      ))
    })
    
    observeEvent(input[["apply_select_statistics" %_% n_table]], {
      rvs$selected_statistics[[n_table]] <- input[["select_statistics" %_% n_table]]
      rvs$transpose_datatable[n_table] <- input[["transpose_datatable" %_% n_table]]
      rvs$remove_na[n_table] <- input[["remove_na" %_% n_table]]
      removeModal()
    })
    
    observeEvent(input[["apply_all_select_statistics" %_% n_table]], {
      # Rep could work too
      for (i in seq_along(rvs$selected_statistics)) {
        rvs$selected_statistics[[i]] <- input[["select_statistics" %_% n_table]]
      }
      rvs$transpose_datatable <- rep(
        input[["transpose_datatable" %_% n_table]],
        rvs$n_table
      )
      rvs$remove_na <- rep(
        input[["remove_na" %_% n_table]],
        rvs$n_table
      )
      removeModal()
    })
    
  })
}