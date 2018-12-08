dqe_verteilungsmodelle_acceptance_sampling_box <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    selectInput(
      inputId = ns("mode"),
      label = NULL,
      choices = label_lang_list(
        de = c("Single Sampling", "OC Curve"),
        en = c("Single Sampling", "OC Curve"),
        value = c("single", "oc")
      )
    ),
    uiOutput(
      outputId = ns("specific_input")
    )
  )
}

dqe_verteilungsmodelle_acceptance_sampling <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("acceptance_sampling", parent, session)

  ns <- session$ns

  rvs <- reactiveValues(
    oc_curve_traces_counter = 1
  )

  output$specific_input <- renderUI({
    if (input$mode == "single") {
      ui <- shiny::tagList(
        fluidRow(
          column(
            width = 4,
            sliderInput(
              inputId = ns("single_p"),
              label = label_lang(
                de = "Fehleranteil p",
                en = "Error-share p"
              ),
              min = 0,
              max = 1,
              value = 0.1
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = ns("single_n"),
              label = label_lang(
                de = "Stichprobenumfang n",
                en = "Sample size n"
              ),
              min = 1,
              value = 10
            )
          ),
          column(
            width = 4,
            numericInput(
              inputId = ns("single_c"),
              label = label_lang(
                de = "Anzahl defekter Einheiten c",
                en = "Number defective c"
              ),
              min = 0,
              max = 10,
              value = 3
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("single_select_prob"),
              label = NULL,
              choices = label_lang_list(
                de = c("Annahmewahrscheinlichkeit", "Ablehnwahrscheinlichkeit"),
                en = c("Acceptance probability", "Decline probability"),
                value = c("accept", "decline")
              )
            )
          ),
          column(
            width = 6,
            textOutput(
              outputId = ns("single_prob")
            )
          )
        )
      )
    } else if (input$mode == "oc") {
      oc_curve_traces_input <- shiny::tagList()
      for (i in seq_len(rvs$oc_curve_traces_counter)) {
        if (i == 1) {
          label_n = label_lang(
            de = "Stichprobenumfang n",
            en = "Sample size n"
          )
          label_c = label_lang(
            de = "Anzahl defekter Einheiten c",
            en = "Number defective c"
          )
        } else {
          label_n = NULL
          label_c = NULL
        }
        oc_curve_traces_input[[i]] <- fluidRow(
          column(
            width = 6,
            numericInput(
              inputId = ns("oc_n" %_% i),
              label = label_n,
              min = 1,
              value = 10
            )
          ),
          column(
            width = 6,
            numericInput(
              inputId = ns("oc_c" %_% i),
              label = label_c,
              min = 0,
              value = 3
            )
          )
        )
      }
      ui <- shiny::tagList(
        oc_curve_traces_input,
        fluidRow(
          column(
            width = 4,
            actionButton(
              inputId = ns("add_oc_curve"),
              label = label_lang(
                de = "OC-Curve",
                en = "OC-Curve"
              )
            )
          ),
          column(
            width = 4,
            actionButton(
              inputId = ns("add_oc_curve_trace"),
              label = label_lang(
                de = "Zusätzliche Kurve",
                en = "Additional curve"
              )
            )
          ),
          column(
            width = 4,
            actionButton(
              inputId = ns("remove_oc_curve_trace"),
              label = label_lang(
                de = "Entferne Kurve",
                en = "Remove curve"
              )
            )
          )
        )
      )
    }
    ui
  })

  observeEvent(input$single_n, {
    updateNumericInput(
      session = session,
      inputId = "single_c",
      max = input$single_n
    )
  })

  output$single_prob <- renderText({
    req(input$single_select_prob)
    if (input$single_select_prob == "accept") {
      pbinom(q = input$single_c, size = input$single_n, prob = input$single_p)
    } else {
      1 - pbinom(q = input$single_c, size = input$single_n, prob = input$single_p)
    }
  })

  observeEvent(input$add_oc_curve, {
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = "OC-Curve",
        plotlyOutput(
          outputId = ns("oc_curve")
        )
      )
    )
  })

  observeEvent(input$add_oc_curve_trace, {
    counter <- rvs$oc_curve_traces_counter
    current_cn <- data.table(c = rep(0, counter), n = rep(0, counter))
    for (i in seq_len(counter)) {
      set(
        current_cn,
        i = i, j = 1:2,
        list(input[["oc_c" %_% i]], input[["oc_n" %_% i]])
      )
    }
    rvs$oc_curve_traces_counter <- counter + 1
    for (i in seq_len(counter)) {
      updateNumericInput(
        session = session,
        inputId = "oc_c" %_% i,
        value = current_cn[i,1]
      )
      updateNumericInput(
        session = session,
        inputId = "oc_n" %_% i,
        value = current_cn[i,2]
      )
    }
  })

  observeEvent(input$remove_oc_curve_trace, {
    counter <- rvs$oc_curve_traces_counter
    if (counter > 1) {
      current_cn <- data.table(c = rep(0, counter - 1), n = rep(0, counter - 1))
      for (i in seq_len(counter - 1)) {
        set(
          current_cn,
          i = i, j = 1:2,
          list(input[["oc_c" %_% i]], input[["oc_n" %_% i]])
        )
      }
      rvs$oc_curve_traces_counter <- rvs$oc_curve_traces_counter - 1
    }
    for (i in seq_len(counter - 1)) {
      updateNumericInput(
        session = session,
        inputId = "oc_c" %_% i,
        value = current_cn[i,1]
      )
      updateNumericInput(
        session = session,
        inputId = "oc_n" %_% i,
        value = current_cn[i,2]
      )
    }
  })

  oc_curve_plot <- reactive({
    counter <- rvs$oc_curve_traces_counter
    data <- tibble(
      x = seq(0, 1, length.out = 100)
    )
    for (i in seq_len(counter)) {
      data[["y" %_% i]] <- pbinom(
        q = req(input[["oc_c" %_% i]]),
        size = req(input[["oc_n" %_% i]]),
        prob = data$x
      )
    }
    p <- plot_ly(data, x = ~x)
    for (i in seq_len(counter)) {
      p <- add_trace(
        p = p,
        y = data[["y" %_% i]],
        name = paste0(i, " n: ", input[["oc_n" %_% i]],
                      ", c: ", input[["oc_c" %_% i]]),
        type = "scatter",
        mode = "lines"
      )
    }
    p <- p %>%
      layout(
        xaxis = list(
          title = label_lang(
            de = "Fehleranteil p",
            en = "Error-share p"
          )
        ),
        yaxis = list(
          title = label_lang(
            de = "Annahmewahrscheinlichkeit",
            en = "Acceptance probability"
          )
        )
      )
  })

  output$oc_curve <- renderPlotly({
    return(oc_curve_plot())
  })
}
