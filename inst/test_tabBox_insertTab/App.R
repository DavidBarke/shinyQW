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
