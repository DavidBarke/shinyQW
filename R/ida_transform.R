#' @export
ida_transform_ui <- function(id) {
  ns <- NS(id)

  dashboardPage(
    skin = "black",
    dashboardHeader(

    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Grundlegende dplyr-Funktionen",
          tabName = ns("dplyr_standard")
        ),
        menuItem(
          "Editor",
          tabName = ns("dplyr_editor")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = ns("dplyr_standard"),
          ida_transform_dplyr_standard_ui(
            id = ns("id_ida_transform_dplyr_standard")
          )
        ),
        tabItem(
          tabName = ns("dplyr_editor"),
          ida_transform_dplyr_editor_ui(
            id = ns("id_ida_transform_dplyr_editor")
          )
        )
      )
    )
  )
}

#' @export
ida_transform <- function(
  input, output, session, data, values, parent, ...
) {
  self <- node$new("transform", parent, session)

  ns <- session$ns

  call_ida_transform_dplyr_standard <- callModule(
    module = ida_transform_dplyr_standard,
    id = "id_ida_transform_dplyr_standard",
    data = data,
    values = values,
    parent = self
  )

  call_ida_transform_dplyr_editor <- callModule(
    module = ida_transform_dplyr_editor,
    id = "id_ida_transform_dplyr_editor",
    data = data,
    values = values,
    parent = self
  )
}
