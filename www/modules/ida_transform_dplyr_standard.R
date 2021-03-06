# TODO: Von Modal auf TabPanel umstellen, dadurch können auch mehrere Datensätze
# gleichzeitig bearbeitet werden

#' @export
ida_transform_dplyr_standard_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      select_data_ui(
        id = ns("id_select_data")
      ),
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = ns("dplyr_function"),
            label = "Funktion",
            choices = list("select", "arrange",
                           "filter", "mutate",
                           "group_by", "summarise")
          )
        ),
        column(
          width = 6,
          actionButton(
            inputId = ns("start_function_dialog"),
            label = "Funktion anwenden"
          )
        )
      )
    ),
    column(
      width = 8,
      tabsetPanel(
        id = ns("tabset")
      )
    )
  )
}

#' @export
ida_transform_dplyr_standard <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("dplyr_standard", parent, session)

  ns <- session$ns

  dplyr_function <- reactiveValues()

  observeEvent(input$start_function_dialog, {
    selector <- paste0("#", ns("tabset"), " + div .active")
    insertUI(
      selector = selector,
      where = "beforeEnd",
      ui = function_dialog_ui(id = ns("id_function_dialog"))
    )
  })

  observeEvent(input$dplyr_function, {
    dplyr_function$dplyr_function <- input$dplyr_function
  })

  # Funktionsmodule

  call_select_data <- callModule(module = select_data,
                                 id = "id_select_data",
                                 data_rvs = .data,
                                 .values = .values,
                                 parent = self,
                                 tabset_data = tibble(
                                   id = c("tabset", "tabset"),
                                   session = c(session, session),
                                   type = c("append_new_tab",
                                            "append_current_tab"),
                                   position = c(1, 2),
                                   label = c("Im neuen Tab anzeigen",
                                             "Im gegenwärtigen Tab anzeigen")
                                 ),
                                 select_column = FALSE
  )

  # Strukturmodule

  call_function_dialog <- callModule(module = function_dialog,
                                     id = "id_function_dialog",
                                     .data = .data,
                                     .values = .values,
                                     parent = self,
                                     selected_data = call_select_data,
                                     dplyr_function = dplyr_function
                                    )
}
