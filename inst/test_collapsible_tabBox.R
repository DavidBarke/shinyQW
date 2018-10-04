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
