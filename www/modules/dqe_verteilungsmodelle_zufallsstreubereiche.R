dqe_verteilungsmodelle_zufallsstreubereiche_box <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        radioButtons(
          inputId = ns("nseitig"),
          label = label_lang(
            de = "Art",
            en = "Type"
          ),
          choices = label_lang_list(
            de = c("Einseitig", "Zweiseitig"),
            en = c("One-sided", "Two-sided"),
            value = c("one_sided", "two_sided")
          ),
          inline = TRUE
        )
      ),
      column(
        width = 6,
        numericInput(
          inputId = ns("alpha"),
          label = label_lang(
            de = "Signifikanzniveau",
            en = "Level of significance"
          ),
          value = 0.05,
          min = 0,
          max = 1
        )
      )
    ),
    module_verteilungen_input_ui(
      id = ns("id_module_verteilungen_input")
    ),
    fluidRow(
      column(
        width = 4,
        actionButton(
          inputId = ns("add_plot"),
          label = label_lang(
            de = "Plot",
            en = "Plot"
          )
        )
      ),
      column(
        width = 4,
        module_verteilungen_input_add_row_button(
          id = ns("id_module_verteilungen_input")
        )
      )
    )
  )
}

dqe_verteilungsmodelle_zufallsstreubereiche <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("zufallsstreubereiche", parent, session)
  
  ns <- session$ns
  
  call_module_verteilungen_input <- callModule(
    module = module_verteilungen_input,
    id = "id_module_verteilungen_input",
    .data = .data,
    .values = .values,
    parent = self
  )
  
  observeEvent(input$add_plot, {
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = "Zufallsstreubereich",
          en = "Random scattering range"
        ),
        plotlyOutput(
          outputId = ns("plot_zufallsstreubereich")
        )
      )
    )
  })
  
  output$plot_zufallsstreubereich <- renderPlotly({
    p <- plot_ly(data = data, x = ~x, y = ~density, type = "scatter", 
                 mode = "lines", fill = "tozeroy") %>%
      layout(
        xaxis = list(
          title = "x"
        ),
        yaxis = list(
          title = "f(x)"
        )
      )
    return(p)
  })
}