dqe_design_of_experiments_uebersicht_box <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      inputId = ns("select_application"),
      label = label_lang(
        de = "Wähle Anwendung",
        en = "Select application"
      ),
      choices = label_lang_list(
        de = c("Pareto-Plot", "Effekt-Plot"),
        en = c("Pareto plot", "Effect plot"),
        value = c("pareto", "effect")
      )
    )
  )
}

dqe_design_of_experiments_uebersicht <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("design_of_experiments_uebersicht", session, parent)
  
  ns <- session$ns
  
  observeEvent(input$open_application, {
    switch(
      input$select_application,
      "pareto" = {
        
      },
      "effect" = {
        
      }
    )
  })
}