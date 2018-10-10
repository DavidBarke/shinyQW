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
      if (type == "guess") {
        if ("jsHooks" %in% names(private$plots[[name]])) {
          session_output$output[[outputId]] <- renderPlotly({
            plot
          })
        } else {
          session_output$output[[outputId]] <- renderPlot({
            plot
          })
        }
      } else if (type == "default") {
        session_output$output[[outputId]] <- shiny::renderPlot({
          plot
        })
      } else {
        session_output$output[[outputId]] <- plotly::renderPlotly({
          plot
        })
      }
    }
  ),
  private = list(
    plots = list()
  )
)

plot_storage <- plot_storage$new()

plot_1 <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x = x, y = y)) + geom_point()
plot_2 <- ggplot(data.frame(x = 1:10, y = 1:10), aes(x = x, y = y)) + geom_line()

plot_storage$set_plot(plot_1, "point")
plot_storage$set_plot(plot_2, "line")

ui <- fluidPage(
  plotOutput(
    outputId = "plot"
  ),
  selectInput(
    inputId = "type",
    label = "Typ",
    choices = c("point", "line")
  )
)

server <- function(input, output, session) {

  observeEvent(input$type, {
    plot_storage$render_plot(input$type, "plot", session_name = NULL)
  })
}

shinyApp(ui, server)
