# Frage: Wie erzeugt man eine collapsible tabBox?
# Antwort: Indem man die tabBox in eine collapsible box packt.

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(

  ),
  dashboardBody(
    box(
      title = "Daten",
      collapsible = TRUE,
      tabBox(
        width = 12,
        tabPanel(
          title = "mtcars",
          "Tabelle"
        ),
        tabPanel(
          title = "diamonds"
        )
      )
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)

h <- box(
  title = "Daten",
  collapsible = TRUE,
  tabBox(
    width = 12,
    tabPanel(
      title = "mtcars",
      "Tabelle"
    ),
    tabPanel(
      title = "diamonds"
    )
  )
)
