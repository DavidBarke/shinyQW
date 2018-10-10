# Hinweis: Grundsätzlich der gleiche Test wie list_of_reactives

ui <- fluidPage(
  actionButton(
    inputId = "a",
    label = "print a"
  ),
  actionButton(
    inputId = "b",
    label = "trigger b"
  )
)

server <- function(input, output, session) {
  rvs <- list(
    inside = reactiveValues(
      a = 1,
      b = 2
    ),
    second = reactiveValues(
      c = 3,
      d = 4
    )
  )

  observeEvent(input$a, {
    print(rvs$inside$a)
  })

  observeEvent(rvs$inside$a, {
    print("rvs$inside$a")
  })

  observeEvent(input$b, {
    rvs$inside$b <- rvs$inside$b + 1
  })

}

shinyApp(ui, server)
