# Frage: Ist es möglich und sinnvoll alle Plots in einem R6-Objekt zu speichern,
# auf das dann von überall in der App zugegriffen werden kann?
# Motivation: renderPlot wird von diesem Objekt übernommen. Dadurch ergibt sich
# eine striktere Trennung von Erstellen und Rendern des Plotes, wodurch
# bestehende Plots einfach modifiziert werden können.
# Antwort: Möglich auf jedenfall, allerdings muss man bei renderPlot mit den
# beiden sessions aufpassen.
library(shiny)
library(R6)
library(ggplot2)

plot_storage <- R6::R6Class(
  classname = "plot_storage",
  public = list(
    initialize = function() {
    },
    set_plot = function(
      plot, name, session = shiny::getDefaultReactiveDomain()
    ) {
      stopifnot(is.character(name))
      if (is.null(session)) {
        private$plots$global[[name]] <- plot
      } else {
        private$plots$session[[session$ns(name)]] <- plot
      }
    },
    get_plot = function(
      name, session = shiny::getDefaultReactiveDomain()
    ) {
      stopifnot(is.character(name))
      if (is.null(session)) {
        return(private$plots$global[[name]])
      } else {
        return(private$plots$session[[session$ns(name)]])
      }
    },
    render_plot = function(
      name, outputId, session_output = shiny::getDefaultReactiveDomain(),
      session_name = shiny::getDefaultReactiveDomain(),
      type = c("default", "plotly", "guess")
    ) {
      type <- match.arg(type)
      if (is.null(session_name)) {
        plot <- private$plots$global[[name]]
      } else {
        plot <- private$plots$session[[session_name$ns(name)]]
      }
      render_default <- TRUE
      if (type == "guess") {
        if ("jsHooks" %in% names(private$plots[[name]])) {
          render_default <- FALSE
        }
      } else if (type == "plotly") {
        render_default <- FALSE
      }
      if (render_default) {
        if (is.function(plot)) {
          session_output$output[[outputId]] <- shiny::renderPlot({
            plot()
          })
        } else {
          session_output$output[[outputId]] <- shiny::renderPlot({
            plot
          })
        }
      } else {
        if (is.function(plot)) {
          session_output$output[[outputId]] <- plotly::renderPlotly({
            plot()
          })
        } else {
          session_output$output[[outputId]] <- plotly::renderPlotly({
            plot
          })
        }
      }
    }
  ),
  private = list(
    plots = list(
      # Wichtig: Kindelemente müssen deklariert sein, da der $-Operator nicht
      # wie von Listen gewohnt rekursiv angewendet werden kann
      global = list(),
      session = list()
    )
  )
)

plot_storage <- plot_storage$new()

ui <- fluidPage(
  plotOutput(
    outputId = "plot"
  ),
  selectInput(
    inputId = "type",
    label = "Typ",
    choices = c("point", "line")
  ),
  numericInput(
    inputId = "x",
    label = "x",
    value = 10,
    min = 2
  )
)

server <- function(input, output, session) {

  plot_1 <- reactive({
    ggplot(data.frame(x = 1:input$x, y = 1:input$x), aes(x = x, y = y)) + geom_point()
  })
  plot_2 <- reactive({
    ggplot(data.frame(x = 1:input$x, y = 1:input$x), aes(x = x, y = y)) + geom_line()
  })

  plot_storage$set_plot(plot_1, "point")
  plot_storage$set_plot(plot_2, "line")

  observeEvent(input$type, {
    plot_storage$render_plot(input$type, "plot")
  })
}

shinyApp(ui, server)
