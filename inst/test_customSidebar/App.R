library(shiny)
library(shinydashboard)
library(shinyQW)

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(
    sidebarMenu(
      actionItem(
        inputId = "action_1",
        label = "Action_1",
        actionSubItem(
          inputId = "action_sub",
          label = "Action_Sub"
        ),
        actionSubItem(
          inputId = "action_sub_2",
          label = "Action_Sub_2"
        )
      ),
      actionItem(
        inputId = "action_2",
        label = "Action_2"
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
