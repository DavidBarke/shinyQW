# Hinweis: Erste Idee, Plots oder Daten in einer einzigen tabBox darzustellen,
# die Verwendung eines R6-Objektes ist aber besser, da sie alle Methoden bereit-
# stellt und dadurch sicherer zu benutzen ist.

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(

  ),
  dashboardBody(
    tabBox(
      id = "tabbed",
      tabPanel(title = "Daten"),
      tabPanel(title = "Plot")
    ),
    actionButton(
      inputId = "insert",
      label = "Insert"
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$insert, {
    appendTab(
      inputId = "tabbed",
      tab = tabPanel(title = "Insertion")
    )
  })
}

shinyApp(ui, server)
