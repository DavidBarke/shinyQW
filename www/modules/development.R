development_tabPanel <- function(id) {
  ns <- NS(id)
  
  list(
    tabPanel(
      title = label_lang(
        de = "Allgemein",
        en = "General"
      ),
      value = "general",
      jsoneditOutput(
        outputId = ns("session_tree")
      )
    )
  )
}

development <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("development", parent, session)
  
  ns <- session$ns
  
  output$session_tree <- renderJsonedit({
    l <- .values$session$tree$create_list()
    jsonedit(l)
  })
  
}