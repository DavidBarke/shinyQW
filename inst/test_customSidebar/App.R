library(shiny)
library(shinydashboard)
library(shinyQW)

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(
    sidebarMenu(
      multiple_actionItem(
        inputId_list = list(
          "dqe" = list(
            "deskriptiv",
            "design",
            "modell",
            "verteilungen"
          ),
          "ida" = list(
            "import",
            "tidy",
            "transform",
            "visualize"
          ),
          "einstellungen" = list(
            "allgemein",
            "ggplot2",
            "plotly"
          )
        ),
        label_list = list(
          "DQE" = list(
            "Deskriptive Statistik",
            "Design of Experiments",
            "Modellstrukturen",
            "Verteilungsmodelle"
          ),
          "IDA" = list(
            "Import",
            "Tidy",
            "Transform",
            "Visualize"
          ),
          "Einstellungen" = list(
            "Allgemein",
            "ggplot2",
            "plotly"
          )
        )
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    ),
    tags$script(HTML("
     $(document).ready(function() {
        $('.div-btn-sidebar').on('click', function(){$(this).blur()});
      })
    ")),
    actionButton(
      inputId = "hda",
      label = "HDA"
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
