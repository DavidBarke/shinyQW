dqe_deskriptive_statistik_uebersicht_box <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_default_ui(
      id = ns("id_data_selector"),
      type = "group_dataset"
    ),
    selectInput(
      inputId = ns("select_application"),
      label = label_lang(
        de = "Wähle Anwendung",
        en = "Select application"
      ),
      choices = label_lang_list(
        de = c("Sortierte Daten", "Gruppierte Daten", "Histogramm", "Boxplot"),
        en = c("Sorted Data", "Grouped Data", "Histogramm", "Boxplot"),
        value = c("sorted_data", "grouped_data", "histogramm", "boxplot")
      )
    ),
    actionButton(
      inputId = ns("open_application"),
      label = label_lang(
        de = "Öffne Anwendung",
        en = "Open application"
      )
    )
  )
}

dqe_deskriptive_statistik_uebersicht <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("overview", parent, session)
  
  ns <- session$ns
  
  observeEvent(input$open_application, {
    content_element_id <- "tab_deskriptive_statistik_element"
    if (input$select_application == "sorted_data") {
      .values$viewer$content$update_tab(
        content_element_id = content_element_id,
        selected = "sorted_data"
      )
    } else if (input$select_application == "grouped_data") {
      .values$viewer$content$update_tab(
        content_element_id = content_element_id,
        selected = "grouped_data"
      )
    } else if (input$select_application == "boxplot") {
      .values$viewer$content$update_tab(
        content_element_id = content_element_id,
        selected = "boxplot"
      )
    }
  })
  
  selected_data <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .data = .data,
    .values = .values,
    parent = self
  )
}