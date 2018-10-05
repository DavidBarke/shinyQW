# Test der closeable collapsible_tabBox()

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(

  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    ),
    collapsible_tabBox(
      tabPanel(
        title = "Panel 1"
      ),
      tabPanel(
        title = "Panel 2"
      ),
      closeable = TRUE,
      title = "TabBox"
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
