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
ida_transform <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                          parent, ...) {
  self <- node$new("transform", parent, session)

  ns <- session$ns

  call_ida_transform_dplyr_standard <- callModule(module = ida_transform_dplyr_standard,
                               id = "id_ida_transform_dplyr_standard",
                               user_data_storage = user_data_storage,
                               permanent_data_storage = permanent_data_storage,
                               values = values,
                               parent = self)

  call_ida_transform_dplyr_editor <- callModule(module = ida_transform_dplyr_editor,
                                                id = "id_ida_transform_dplyr_editor",
                                                user_data_storage = user_data_storage,
                                                permanent_data_storage = permanent_data_storage,
                                                values = values,
                                                parent = self)
}
