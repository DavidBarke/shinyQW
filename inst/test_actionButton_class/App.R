# Frage: Funktioniert shinyjs::addClass bei actionButtons
# Antwort: Ja
# Motivation: Hat bei einem schließenden actionButton nicht funktioniert, da
# der actionButton noch nicht im DOM war

ui <- fluidPage(
  useShinyjs(),
  actionButton(
    "id",
    "Label"
  )
)

server <- function(input, output, session) {
  addClass(id = "id", class = "custom")
}

shinyApp(ui, server)
