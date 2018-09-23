# Frage: Kann einem Modul eine Liste von reactiveValues übergeben werden?
# Motivation: Übergabe von verschiedenen reactiveValues, die zum Beispiel unterschiedliche
# Arten von Daten speichern an select_data()

library(shiny)

module <- function(input, output, session, rvs) {
  observe(rvs, {
    print(rvs[[1]])
  })
}

ui <- fluidPage(

)

server <- function(input, output, session) {
  rvs_1 <- reactiveValues(x = 1)
  rvs_2 <- reactiveValues(y = 1)

  callModule(module, "id_module", rvs = list(rvs_1, rvs_2))
}

shinyApp(ui, server)
