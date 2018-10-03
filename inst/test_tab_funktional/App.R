library(shiny)
library(shinydashboard)
library(shinyQW)

create_subItems <- function(text, tabName) {
  ui <- do.call(shinydashboard::menuSubItem,
                list(text = text, tabName = tabName))
  return(ui)
}

multiple_menuItem <- function(structure, menuItem_args = NULL,
                              menuSubItem_args = NULL) {
  ui_item <- shiny::tagList()
  for (i in seq_along(structure)) {
    item <- structure[[i]]
    if (is.list(item) && length(item) != 0){
      ui_sub <- purrr::pmap(.l = list(text = item, tabName = names(item)),
                            .f = create_subItems)
      ui_item[[i]] <- do.call(shinydashboard::menuItem, list(
        text = names(structure[i]),
        ui_sub
      ))
    }
  }
  return(ui_item)
}

ui <- dashboardPage(
  dashboardHeader(

  ),
  dashboardSidebar(
    sidebarMenu(
      multiple_menuItem(list(IDA = list(id_import = "Import",
                                        id_transform = "Transform"),
                             DQE = list(id_testtheorie = "Testtheorie")))
    )
  ),
  dashboardBody(

  )
)

server <- function(input, output, session) {

}

shinyApp(ui, server)
