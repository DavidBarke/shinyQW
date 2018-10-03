id_dashboardSidebar <- function(..., disable = FALSE, width = NULL,
                                collapsed = FALSE, id) {
  h <- dashboardSidebar(..., disable, width, collapsed)
  h$attribs$id <- id
  h
}

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(

  ),
  dashboardBody(
    dashboardPage(
      title = "Inside",
      dashboardHeader(

      ),
      id_dashboardSidebar(
        id = "sidebar_2"
      ),
      dashboardBody(

      )
    )
  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
