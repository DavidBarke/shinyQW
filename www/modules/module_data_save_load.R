module_data_save_load_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 6,
        downloadButton(
          outputId = ns("save_data"),
          label = label_lang(
            de = "Speichere Datensätze",
            en = "Save datasets"
          )
        )
      ),
      column(
        width = 6,
        fileInput(
          inputId = ns("load_data"),
          label = label_lang(
            de = "Lade Datensätze",
            en = "Load datasets"
          )
        )
      )
    )
  )
}

module_data_save_load <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("data_save_load", parent, session)
  
  ns <- session$ns
  
  output$save_data <- downloadHandler(
    filename = function() {
      paste0("QW-App-Data-", Sys.Date(), ".zip")
    },
    content = function(file) {
      files <- character()
      object_information <- list()
      groups <- .data$get_group_names()
      
      # Create .rds-file for every dataset
      for (group in groups) {
        datasets <- .data$get_datasets_names(group)
        object_information[[group]] <- list()
        for (dataset in datasets) {
          data <- .data$get_dataset(group, dataset)
          object_information[[group]][[dataset]] <- .data$get_object_information(
            group, dataset
          )
          filepath <- paste0(
            "www/output/", group, "-", dataset, "-", Sys.Date(), ".rds"
          )
          write_rds(data, filepath)
          files <- append(files, filepath)
        }
      }
      
      # Create one .rds-file for metadata object information
      object_information_filepath <- paste0(
        "www/output/object_information-", Sys.Date(), ".rds"
      )
      write_rds(object_information, object_information_filepath)
      files <- append(files, object_information_filepath)
      
      # Create zip-folder
      zip(file, files)
    },
    contentType = "application/zip"
  )
  
  observeEvent(input$load_data, {
    req(input$load_data$type == "application/x-zip-compressed")
    datapath <- input$load_data$datapath
    unzip(datapath, exdir = "www/upload")
    
  })
}