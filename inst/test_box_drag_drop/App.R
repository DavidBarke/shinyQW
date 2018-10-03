library(shiny)
library(shinydashboard)
library(shinyjqui)

full_box <- function(..., title = NULL, footer = NULL, status = NULL,
                     solidHeader = FALSE, background = NULL, width = 6,
                     height = NULL, collapsible = FALSE, collapsed = FALSE) {
  h <- box(..., title = title, footer = footer, status = status,
           solidHeader = solidHeader, background = background, width = width,
           height = height, collapsible = collapsible, collapsed = collapsed)
  h$attribs$style <- "padding: 0px"
  h
}

full_dashboardBody <- function(...) {
  h <- dashboardBody(...)
  h$children[[1]]$attribs$style <- "padding: 0px"
  h
}

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(

  ),
  full_dashboardBody(
    full_box(
      title = "Menuleiste",
      collapsible = TRUE,
      width = 12,
      solidHeader = TRUE,
      box("Box_1"),
      box("Box_2")
    ),
    full_box(
      title = "Box_1"
    ),
    full_box(
      title = "Box_2"
    )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
