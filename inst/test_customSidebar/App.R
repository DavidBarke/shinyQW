# Frage: Kann man die shinydashboard-Sidebar vernünftig manipulieren?
# Antwort: Ja, man muss jedoch auf die HTML-Struktur von dashboardSidebar
# achten.
# Motivation: Alle tabItem in tabItems zu packen, ist nett, aber erlaubt immer
# nur einen offenen Tab. Eine Idee für einen anderen Test wäre eine Liste aus
# tabItems zu erzeugen, die dynamisch mit dem jeweiligen anzuzeigenden tabItem
# befüllt wird. Die jetzige Lösung die Sidebar aus actionButton zusammenzubauen,
# ermöglicht aber normale Methoden (z.B. observeEvent) anzuwenden, um vollkommen
# flexibel zu agieren.
#
# Hinweis: Aus diesem Test sind actionItem und actionSubItem hervorgegangen.

library(shiny)
library(shinydashboard)
library(shinyQW)
library(shinyjqui)

tabList <- tabList_R6$new(id = "placeholder", sortable = TRUE)

viewer_data <- tabBox_R6$new(
  id = "viewer_data",
  title = "Daten",
  width = 12
)

viewer_plot <- tabBox_R6$new(
  id = "viewer_plot",
  title = "Plots",
  width = 12
)

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
    fluidRow(
      column(
        width = 6,
        tabList$container()
      ),
      column(
        width = 6,
        jqui_sortable(
          ui = tags$div(
            viewer_data$tabBox(collapsible = TRUE),
            viewer_plot$tabBox(collapsible = TRUE)
          )
        )
      )
    )
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
