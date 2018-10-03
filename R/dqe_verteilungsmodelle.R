diskrete_verteilungen <- c(
    "Binomialverteilung" = "binom",
    "Geometrische Verteilung" = "geom",
    "Hypergeometrische Verteilung" = "hyper",
    "Multinomialverteilung" = "multinom",
    "Negative Binomialverteilung" = "nbinom",
    "Poissonverteilung" = "pois"
  )

stetige_verteilungen <- c(
  "Betaverteilung" = "beta",
  "Cauchyverteilung" = "cauchy",
  "Chi-Quadrat-Verteilung" = "chisq",
  "Exponentialverteilung" = "exp",
  "F-Verteilung" = "f",
  "Gammaverteilung" = "gamma",
  "Gleichverteilung" = "unif",
  "Log-Normalverteilung" = "lnorm",
  "Normalverteilung" = "norm",
  "t-Verteilung" = "t",
  "Weibullverteilung" = "weibull"
)

#' @export
dqe_verteilungsmodelle_ui <- function(id) {
  ns <- NS(id)
  dashboardPage(
    skin = "black",
    dashboardHeader(

    ),
    dashboardSidebar(
      sidebarMenu(
        menuItem(
          "Theorie",
          tabName = ns("theorie")
        ),
        menuItem(
          "Verteilungen",
          tabName = ns("verteilungen")
        ),
        menuItem(
          "Vergleich mehrerer Verteilungen",
          tabName = ns("vergleich")
        ),
        menuItem(
          "Test",
          tabName = ns("test")
        )
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(
          tabName = ns("test"),
          sidebarLayout(
            sidebarPanel(
              uiOutput(
                outputId = ns("ui_test")
              )
            ),
            mainPanel(
              uiOutput(
                outputId = ns("ui_plot_list")
              )
            )
          )
        ),
        tabItem(
          tabName = ns("verteilungen"),
          div(
            class = "row",
            actionButton(
              inputId = ns("add_verteilungen_plot"),
              label = "Add Plot"
            ),
            div(
              id = "container_verteilungen_plots"
            )
          )
        ),
        tabItem(
          tabName = ns("vergleich"),
          sidebarLayout(
            sidebarPanel(

            ),
            mainPanel(

            )
          )
        ),
        tabItem(
          tabName = ns("theorie"),
          div(
            class = "theorie",
            h3("Diskrete Verteilungen"),
            hr(),
            h3("Stetige Verteilungen")
          )
        )
      )
    )
  )
}

dqe_verteilungsmodelle_box <- function(id) {
  ns <- NS(id)

  div("Temporary UI")
}

#' @export
dqe_verteilungsmodelle <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                   parent, ...) {
  self <- node$new("verteilungsmodelle", parent, session)

  ns <- session$ns

# VERTEILUNGEN ----------------------------------------------------------------------------------------

  observeEvent(input$add_verteilungen_plot, {
    # Counter initialisieren
    if (!"plots" %in% names(rvs_verteilungen$counter)) {
      rvs_verteilungen$counter$plots <- 1
    }
    count <- rvs_verteilungen$counter$plots
    # IDs
    uiDivId <- paste("div_verteilungen_sidebar_plot_input", count, sep = "_")
    selectVerteilungPlotId <- paste(uiDivId, "select_verteilung_plot", sep = "_")
    selectTypePlotId <- paste(uiDivId, "select_type_plot", sep = "_")
    # Neues ui_element
    input_element <- div(
      id = uiDivId,
      fluidRow(
        column(
          width = 6,
          selectInput(
            inputId = ns(selectVerteilungPlotId),
            label = "Verteilung",
            choices = list("Diskrete Verteilungen" = diskrete_verteilungen,
                           "Stetige Verteilungen" = stetige_verteilungen
            )
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = ns(selectTypePlotId),
            label = "Art des Plots",
            choices = list("Dichtefunktion" = "density", "Verteilungsfunktion" = "probability", "Quantilsfunktion" = "quantile")
          )
        )
      )
    )
    # Output-Element
    outputId <- paste("o_verteilungen_plot", count, sep = "_")
    output_element <- plotOutput(
      outputId = ns(outputId)
    )
    # UI-Element
    ui_element <- div(
      div(
        class = "col-sm-4",
        input_element
      ),
      div(
        class = "col-sm-8",
        output_element
      )
    )
    insertUI(
      selector = "#container_verteilungen_plots",
      where = "beforeEnd",
      ui = ui_element
    )
    # Output-Logik
    output[[outputId]] <- renderPlot({
      plot <- get_distribution_plot(
        input = input,
        values = values,
        prefix_id = uiDivId,
        distribution = input[[selectVerteilungPlotId]],
        type = input[[selectTypePlotId]],
        plot_engine = "ggplot2"
      )
      return(plot)
    })
    # Neuer Listeneintrag im reactiveValues, das die spezifischen InputWidgets hält
    rvs_verteilungen$specific_distribution_input[[uiDivId]] <- list()
    # Observer
    observeEvent(input[[selectVerteilungPlotId]], {
      if (!input[[selectVerteilungPlotId]] %in% names(rvs_verteilungen$specific_distribution_input[[uiDivId]])) {
        rvs_verteilungen$specific_distribution_input[[uiDivId]][[input[[selectVerteilungPlotId]]]] <- 1
        insertUI(
          selector = paste("#", uiDivId, sep = ""),
          where = "beforeEnd",
          ui = get_specific_distribution_input(
            session = session,
            input = input,
            values = values,
            prefix_id = uiDivId,
            distribution = input[[selectVerteilungPlotId]]
          )
        )
      }
      hide_all_specific_distribution_inputs(
        prefix_id = uiDivId
      )
      show_specific_distribution_input(
        prefix_id = uiDivId,
        distribution = input[[selectVerteilungPlotId]]
      )
    })
    # Counter inkrementieren
    rvs_verteilungen$counter$plots <- rvs_verteilungen$counter$plots + 1
  })

  rvs_verteilungen <- reactiveValues()
}
