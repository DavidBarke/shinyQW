data_import_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(
      outputId = ns("step_1")
    ),
    uiOutput(
      outputId = ns("step_2")
    ),
    uiOutput(
      outputId = ns("step_3")
    ),
    uiOutput(
      outputId = ns("prev_step"),
      inline = TRUE
    ),
    uiOutput(
      outputId = ns("next_step"),
      inline = TRUE
    )
  )
}

data_import <- function(
  input, output, session, .data, .values, parent, ...
) {
  
  self <- node$new("data_import", parent, session)
  
  ns <- session$ns
  
  .max_step <- 3
  
  rvs <- reactiveValues(
    step = 1,
    allowed_steps = 1,
    ending = character(),
    ending_supported = FALSE,
    live_data_supported = TRUE,
    excecute_import = FALSE
  )
  
  output$prev_step <- renderUI({
    if (rvs$step > 1 && (rvs$step - 1) %in% rvs$allowed_steps) {
      ui <- actionButton(
        inputId = ns("prev_step"),
        label = "<"
      )
      return(ui)
    }
  })
  
  output$next_step <- renderUI({
    if (rvs$step < .max_step && (rvs$step + 1) %in% rvs$allowed_steps) {
      ui <- actionButton(
        inputId = ns("next_step"),
        label = ">"#,
        #class = "pull-right"
      )
      return(ui)
    }
  })
  
  observeEvent(input$prev_step, {
    designated_step <- max(1, rvs$step - 1)
    if (designated_step %in% rvs$allowed_steps) {
      rvs$step <- designated_step
    }
  })
  
  observeEvent(input$next_step, {
    designated_step <- min(.max_step, rvs$step + 1)
    if (designated_step %in% rvs$allowed_steps) {
      rvs$step <- designated_step
    }
  })
  
  output$step_1 <- renderUI({
    if (rvs$step == 1) {
      ui <- tagList(
        fileInput(
          inputId = ns("file_input"),
          label = label_lang(
            de = "Wähle Datei",
            en = "Select file"
          ),
          buttonLabel = label_lang(
            de = "Auswählen",
            en = "Browse"
          ),
          placeholder = label_lang(
            de = "Keine Datei ausgewählt",
            en = "No file selected"
          )
        ),
        uiOutput(
          outputId = ns("file_ending")
        )
      )
      return(ui)
    }
  })
  
  file_ending <- reactive({
    file_input <- req(input$file_input)
    datapath <- file_input$datapath
    ending <- stringr::str_extract(datapath, "\\.\\w+$")
    if (ending %in% c(".csv", ".xls", ".xlsx")) {
      rvs$ending_supported <- TRUE
      rvs$ending <- ending
      if (!(2 %in% rvs$allowed_steps)) {
        rvs$allowed_steps <- c(rvs$allowed_steps, 2)
      }
    } else {
      rvs$ending_supported <- FALSE
      rvs$allowed_steps <- rvs$allowed_steps[rvs$allowed_steps != 2]
    }
    ending
  })
  
  datapath <- reactive({
    file_input <- req(input$file_input)
    datapath <- file_input$datapath
  })
  
  output$file_ending <- renderUI({
    file_ending <- file_ending()
    if (rvs$ending_supported == TRUE) {
      ui <- div(
        label_lang(
          de = paste0("Dateiendung ", file_ending, " erkannt"),
          en = paste0("File ending ", file_ending, " recognised")
        ),
        actionButton(
          inputId = ns("begin_import"),
          label = label_lang(
            de = "Mit dem Import beginnen",
            en = "Start the import"
          )
        )
      )
    } else {
      ui <- div(
        label_lang(
          de = paste0("Dateiendung ", file_ending, " wird nicht unterstützt"),
          en = paste0("File ending ", file_ending, " not supported")
        )
      )
    }
    ui
  })
  
  observeEvent(input$begin_import, {
    rvs$step <- 2
  })
  
  output$step_2 <- renderUI({
    if (rvs$step == 2) {
      ui <- tagList(
        uiOutput(
          outputId = ns("specific_input")
        ),
        actionButton(
          inputId = ns("open_preview"),
          label = label_lang(
            de = "Vorschau",
            en = "Preview"
          )
        ),
        actionButton(
          inputId = ns("open_import_modal"),
          label = label_lang(
            de = "Importiere",
            en = "Import"
          )
        )
      )
      return(ui)
    }
  })
  
  output$specific_input <- renderUI({
    ui <- switch(
      rvs$ending,
      ".csv" = data_import_csv_ui(
        id = ns("id_data_import_csv")
      ),
      ".xls" = data_import_excel_ui(
        id = ns("id_data_import_excel")
      ),
      ".xlsx" = data_import_excel_ui(
        id = ns("id_data_import_excel")
      )
    )
  })
  
  observeEvent(input$open_preview, {
    .values$viewer$data$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = "Vorschau",
          en = "Preview"
        ),
        value = ns("preview"),
        uiOutput(
          outputId = ns("preview_ui")
        )
      )
    )
  })
  
  output$preview_ui <- renderUI({
    if (rvs$live_data_supported) {
      ui <- dataTableOutput(
        outputId = ns("preview_datatable")
      )
    } else {
      ui <- label_lang(
        de = "Beim Import mit den vorgegebenen Parametern ist ein Fehler aufgetreten",
        en = "During the import with the selected parameters an error occured"
      )
    }
    return(ui)
  })
  
  output$preview_datatable <- renderDataTable({
    live_data()
  })
  
  live_data <- reactive({
    if (rvs$ending_supported) {
      ending <- str_replace(rvs$ending, "\\.", "")
      if (ending %in% c("xls", "xlsx")) {
        ending <- "excel"
      }
      live_data <- get(paste0("import_", ending))()
    }
  })
  
  observeEvent(rvs$live_data_supported, {
    if (rvs$live_data_supported) {
      if (!(3 %in% rvs$allowed_steps)) {
        rvs$allowed_steps <- c(rvs$allowed_steps, 3)
      }
    } else {
      rvs$allowed_steps <- rvs$allowed_steps[rvs$allowed_steps != 3]
    }
  })
  
  output$step_3 <- renderUI({
    if (rvs$step == 3) {
      ui <- tagList(
        selectInput(
          inputId = ns("dataset_groups"),
          label = label_lang(
            de = "Gruppe des neuen Datensatzes",
            en = "Group of new dataset"
          ),
          choices = .data$get_group_names(),
          multiple = TRUE
        ),
        textInput(
          inputId = ns("dataset_name"),
          label = label_lang(
            de = "Name des neuen Datensatzes",
            en = "Name of new dataset"
          )
        ),
        uiOutput(
          outputId = ns("finish_import")
        )
      )
      return(ui)
    }
  })
  
  output$finish_import <- renderUI({
    actionButton(
      inputId = ns("finish_import"),
      label = label_lang(
        de = paste0(
          "Ausgewählte Datei der Gruppe ", req(input$dataset_groups), 
          " als Datensatz ", req(input$dataset_name), " hinzufügen"
        ),
        en = paste0(
          "Import selected file as part of group ", req(input$dataset_groups),
          " as dataset ", req(input$dataset_name)
        )
      )
    )
  })
  
  observeEvent(input$finish_import, {
    groups <- input$dataset_groups
    groups_with_same_name <- character()
    for (group in groups) {
      if (input$dataset_name %in% .data$get_datasets_names(group)) {
        groups_with_same_name <- append(groups_with_same_name, group)
      }
    }
    if (length(groups_with_same_name) > 0) {
      showModal(modalDialog(
        title = label_lang(
          de = "Bestätigung des Namens des Datensatzes",
          en = "Confirmation of dataset name"
        ),
        paste0(
          group_string, 
          " existiert bereits ein Datensatz mit dem Namen ",
          input$dataset_name,
          ":"
        ),
        groups_with_same_name,
        footer = tagList(
          modalButton(
            label = label_lang(
              de = "Verwerfen",
              en = "Dismiss"
            )
          ),
          actionButton(
            inputId = ns("overwrite_datasets"),
            label = label_lang(
              de = "Datensätze überschreiben",
              en = "Overwrite datasets"
            )
          )
        )
      ))
    }
    rvs$execute_import <- TRUE
  })
  
  observeEvent(input$overwrite_datasets, {
    rvs$execute_import <- TRUE
  })
  
  observeEvent(rvs$execute_import, {
    if (rvs$execute_import) {
      rvs$execute_import <- FALSE
      groups <- input$dataset_groups
      for (group in groups) {
        .data$add_dataset(group, input$dataset_name, live_data())
      }
    }
  })
  
  import_csv <- callModule(
    module = data_import_csv,
    id = "id_data_import_csv",
    .data = .data,
    .values = .values,
    parent = self,
    datapath = datapath
  )
  
  import_excel <- callModule(
    module = data_import_excel,
    id = "id_data_import_excel",
    .data = .data,
    .values = .values,
    parent = self,
    datapath = datapath
  )
  
}