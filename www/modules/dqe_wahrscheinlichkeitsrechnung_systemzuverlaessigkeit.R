dqe_wahrscheinlichkeitsrechnung_systemzuverlaessigkeit_ui <- function(id) {
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

dqe_wahrscheinlichkeitsrechnung_systemzuverlaessigkeit <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("wahrscheinlichkeitsrechnung_rechner", parent, session)
  
  ns <- session$ns
  
  rvs <- reactiveValues(
    n_table = 0,
    n_row = numeric(),
    max_n_row = numeric()
  )
  
  .envir <- environment()
  
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
    rvs$n_row[n_table] <- 1
    rvs$max_n_row[n_table] <- 1
    
    # New reactive: n_box is integer vector; one value per row representing the
    # number of boxes
    assign(
      "n_box" %_% n_table,
      1,
      pos = .subset2(rvs, "impl")$.values
    )
    
    insertUI(
      selector = paste0("#", ns("input_tables")),
      where = "afterBegin",
      ui = uiOutput(
        outputId = ns("input_table" %_% n_table)
      )
    )
    
    output[["input_table" %_% n_table]] <- renderUI({
      if (input$select_input_table == n_table) {
        prob_rows <- tagList()
        for (i in seq_len(rvs$n_row[n_table])) {
          prob_row_boxes <- tagList()
          for (j in seq_len(rvs[["n_box" %_% n_table]][i])) {
            prob_row_boxes[[j]] <- column(
              width = 2,
              numericInput(
                inputId = ns("prob_box" %_% n_table %_% i %_% j),
                label = paste0("Prob ", i, "-", j),
                value = fallback(
                  input[["prob_box" %_% n_table %_% i %_% j]], 
                  0.5
                ),
                min = 0,
                max = 1,
                step = 0.1
              )
            ) 
          }
          prob_rows[[i]] <- fluidRow(
            column(
              width = 10,
              fluidRow(prob_row_boxes)
            ),
            column(
              width = 2,
              fluidRow(
                column(
                  width = 6,
                  actionButton(
                    inputId = ns("add_box" %_% n_table %_% i),
                    label = "+"
                  )
                ),
                column(
                  width = 6,
                  actionButton(
                    inputId = ns("remove_box" %_% n_table %_% i),
                    label = "-"
                  )
                )
              )
            )
          )
        }
        ui <- tagList(
          prob_rows,
          fluidRow(
            column(
              width = 4,
              actionButton(
                inputId = ns("add_row" %_% n_table),
                label = label_lang(
                  de = "Neue Zeile",
                  en = "Add row"
                )
              )
            ),
            column(
              width = 4,
              actionButton(
                inputId = ns("remove_row" %_% n_table),
                label = label_lang(
                  de = "Entferne Zeile",
                  en = "Remove row"
                )
              )
            )
          )
        )
        return(ui)
      }
    })
    
    observeEvent(input[["add_row" %_% n_table]], {
      rvs$n_row[n_table] <- rvs$n_row[n_table] + 1
      n_row <- rvs$n_row[n_table]
      
      # rvs$max_n_row guarantees that observeEvent is only called once
      if (n_row > rvs$max_n_row[n_table]) {
        rvs$max_n_row[n_table] <- rvs$max_n_row[n_table] + 1
        
        rvs[["n_box" %_% n_table]][n_row] <- 1
        
        observeEvent(input[["add_box" %_% n_table %_% n_row]], {
          rvs[["n_box" %_% n_table]][n_row] <- 
            rvs[["n_box" %_% n_table]][n_row] + 1
        })
        
        observeEvent(input[["remove_box" %_% n_table %_% n_row]], {
          rvs[["n_box" %_% n_table]][n_row] <- 
            max(1, rvs[["n_box" %_% n_table]][n_row] - 1)
        })
      }
    })
    
    # First add_box observer has to be declared manually
    observeEvent(input[["add_box" %_% n_table %_% 1]], {
      rvs[["n_box" %_% n_table]][1] <- rvs[["n_box" %_% n_table]][1] + 1
    })
    
    observeEvent(input[["remove_row" %_% n_table]], {
      rvs$n_row[n_table] <- max(1, rvs$n_row[n_table] - 1)
    })
    
    # First remove_box observer has to be declared manually
    observeEvent(input[["remove_box" %_% n_table %_% 1]], {
      rvs[["n_box" %_% n_table]][1] <- max(1, rvs[["n_box" %_% n_table]][1] - 1)
    })
  })
  
}
