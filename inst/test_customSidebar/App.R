library(shiny)
library(shinydashboard)
library(shinyQW)

tabList <- tabList_R6$new(id = "placeholder")

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
    tabList$container()
  )
)

server <- function(input, output, session) {

  tabList$set_session(session)

  tabList$add_list_item_by_actionButton(
    ui = tabList$tabBox(
      id = "deskriptive_statistik",
      title = "Deskriptive Statistik",
      tabPanel(
        title = "Sortierte Daten"
      ),
      tabPanel(
        title = "Gruppierte Daten"
      )
    ),
    inputId = "deskriptiv"
  )
  tabList$add_list_item_by_actionButton(
    ui = tabList$tabBox(
      id = "modellstrukturen",
      title = "Modellstrukturen",
      tabPanel(
        title = "Lineares Modell"
      )
    ),
    inputId = "modell"
  )

}

shinyApp(ui, server)
