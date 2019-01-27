# TODO: R6-Layout-Objekt, Manuelle InputIds, Return-Liste möglicherweise als R6

data_selector_layout <- R6Class(
  "data_selector_layout",
  public = list(
    add_custom_layout = function() {
      
    },
    
    get_default_layout = function(name) {
      private$default_layouts[[name]]
    },
    
    get_custom_layout = function(name) {
      private$custom_layouts[[name]]  
    }
  ),
  private = list(
    default_layouts = list(
      "gd" = list(),
      "gdc" = list(),
      "gdm" = list(),
      "gdcm" = list()
    ),
    custom_layouts = list()
  )
)

layout <- list(
  html = function(ns) {
    tagList(
      fluidRow(
        column(
          width = 6,
          uiOutput(
            outputId = ns("select_group_1")
          )
        ),
        column(
          width = 6,
          uiOutput(
            outputId = ns("select_dataset_1")
          )
        )
      )
    )
  },
  structure = tibble(
    group = 1, dataset = 1, column = 0, columns = 0
  )
)

data_selector_2_ui <- function(id) {
  ns <- NS(id)
  
  uiOutput(
    outputId = ns("module_ui")
  )
}

data_selector_2 <- function(
  input, output, session, .data, .values, parent, layout, unique_suffix = NULL,
  ...
) {
  
  self <- node$new(paste0("data_selector_2", unique_suffix), parent, session)
  
  ns <- session$ns
  
  output$module_ui <- renderUI({
    layout$html(ns)
  })
  
  return_list <- list()
  structure <- layout$structure
  
  for (i in seq_len(nrow(structure))) {
    row <- structure[i,]
    
  }
  
  return(return_list)
}
