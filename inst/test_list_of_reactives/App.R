# Frage: Kann einem Modul eine Liste von reactiveValues übergeben werden?
# Motivation: Übergabe von verschiedenen reactiveValues, die zum Beispiel unterschiedliche
# Arten von Daten speichern an select_data
# Ergebnis: reactiveValues müssen als Liste übergeben werden (ACHTUNG: c() funktioniert
# nicht, da c() die Attribute entfernt. Theoretisch wäre es möglich, man müsste dann
# jedoch anstelle von rvs$x rvs$get("x") verwenden, was unintuitiv ist.)

library(shiny)

module <- function(input, output, session, rvs) {
  observe({
    print(rvs$a$z)
  })
}

ui <- fluidPage(

)

server <- function(input, output, session) {
  rvs_1 <- reactiveValues(x = 2, y = 2, z = 3)
  rvs_2 <- reactiveValues(y = 1)

  callModule(module, "id_module", rvs = list(a = rvs_1, b = rvs_2))
}

shinyApp(ui, server)
