library(shiny)
library(shinydashboard)
library(shinyAce)
library(shinyjs)
library(shinyjqui)
library(colourpicker)
library(shinyQW)
library(tidyverse)
library(plotly)
library(R6)
library(R.utils)
library(DT)
library(patchwork)

# SOURCE -----------------------------------------------------------------------
# Damit nicht nach jeder Veränderung shinyQW neu gebuilded werden muss
# Es ist wichtig, dass modifiedOnly = FALSE ist!
sourceDirectory(path = "../../../R", encoding = "UTF-8", modifiedOnly = FALSE)

# Globals ----------------------------------------------------------------------

tabList <- tabList_R6$new(id = "placeholder", sortable = TRUE)

viewer_data <- tabBox_R6$new(
  id = "viewer_data",
  title = "Daten",
  width = 12
)

viewer_plot <- tabBox_R6$new(
  id = "viewer_plot",
  title = "Plots",
  width = 12
)

lehrveranstaltungen <- list(
  ida = c(
    "import",
    "tidy",
    "transform",
    "programming",
    "regular_expressions",
    "visualize"
  ),
  dqe = c(
    "design_of_experiments",
    "deskriptive_statistik",
    "modellstrukturen",
    "statistische_prozesskontrolle",
    "testtheorie",
    "verteilungsmodelle",
    "wahrscheinlichkeitsrechnung"
  )
)

einstellungen <- c("allgemein", "ggplot2", "plotly", "dqe")

# UI ---------------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(
    title = "QW-App"
  ),
  dashboardSidebar(
    sidebarMenu(
      actionItem(
        inputId = "ida",
        label = "IDA",
        actionSubItem(
          "tab_import",
          "Import"
        ),
        actionSubItem(
          "tab_tidy",
          "Tidy"
        ),
        actionSubItem(
          "tab_transform",
          "Transform"
        ),
        actionSubItem(
          "tab_programming",
          "Programming"
        ),
        actionSubItem(
          "tab_regular_expressions",
          "Regular Expressions"
        ),
        actionSubItem(
          "tab_visualize",
          "Visualize"
        )
      ),
      actionItem(
        "dqe",
        "DQE",
        actionSubItem(
          "tab_deskriptive_statistik",
          "Deskriptive Statistik"
        ),
        actionSubItem(
          "tab_wahrscheinlichkeitsrechnung",
          "Wahrscheinlichkeitsrechnung"
        ),
        actionSubItem(
          "tab_verteilungsmodelle",
          "Verteilungsmodelle"
        ),
        actionSubItem(
          "tab_statistische_prozesskontrolle",
          "Statistische Prozesskontrolle"
        ),
        actionSubItem(
          "tab_modellstrukturen",
          "Modellstrukturen"
        ),
        actionSubItem(
          "tab_design_of_experiments",
          "Design of Experiments"
        )
      ),
      actionItem(
        "einstellungen",
        "Einstellungen",
        actionSubItem(
          "tab_allgemein",
          "Allgemein"
        ),
        actionSubItem(
          "tab_ggplot2",
          "ggplot2"
        ),
        actionSubItem(
          "tab_plotly",
          "plotly"
        )
      )
    )
  ),
  full_dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "styles.css"
      )
    ),
    menubar_ui(
      id = "id_menubar",
      title = "Menu"
    ),
    fluidRow(
      column(
        width  = 6,
        tabList$container()
      ), # column
      column(
        width = 6,
        jqui_sortable(
          tags$div(
            viewer_data$tabBox(collapsible = TRUE),
            viewer_plot$tabBox(collapsible = TRUE)
          )
        )
      ) # column
    ) # fluidRow
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  self <- node$new("app", session = session)


# SET SESSION ------------------------------------------------------------------

  tabList$set_session(session)

  viewer_data$set_session(session)
  viewer_plot$set_session(session)

# CONNECT TABLIST WITH ACTIONBUTTONS

  tabList$add_list_item_by_actionButton(
    ui = tabList$tabBox(
      inputId = "tab_deskriptive_statistik",
      title = "Deskriptive Statistik",
      tabPanel_list = dqe_deskriptive_statistik_tabPanel(
        id = "id_deskriptive_statistik"
      )
    ),
    inputId = "tab_deskriptive_statistik"
  )

  tabList$add_list_item_by_actionButton(
    ui = tabList$tabBox(
      inputId = "tab_design_of_experiments",
      title = "Design of Experiments",
      tabPanel_list = dqe_design_of_experiments_tabPanel(
        id = "id_design_of_experiments"
      )
    ),
    inputId = "tab_design_of_experiments"
  )

  tabList$add_list_item_by_actionButton(
    ui = tabList$tabBox(
      inputId = "tab_ggplot2",
      title = "Einstellungen ggplot2",
      tabPanel_list = einstellungen_ggplot2_tabPanel(
        id = "id_einstellungen_ggplot2"
      )
    ),
    inputId = "tab_ggplot2"
  )

# REACTIVE VALUES --------------------------------------------------------------

  .data <- list(
    user_data_storage = reactiveValues(),
    permanent_data_storage = reactiveValues(
      mtcars = mtcars,
      iris = iris,
      cars = cars
    )
  )

  .values <- list(
    session = reactiveValues(
      tree = self
    ),
    viewer = reactiveValues(
      data = viewer_data,
      plot = viewer_plot
    ),
    einstellungen = list(
      allgemein = reactiveValues(),
      ggplot2 = reactiveValues(),
      plotly = reactiveValues(),
      dqe = reactiveValues()
    )
  )

  for (lehrveranstaltung in names(lehrveranstaltungen)) {
    themen <- lehrveranstaltungen[[lehrveranstaltung]]
    call_multiple_modules_2(module_templates = themen,
                            glue_module = list(x1 = lehrveranstaltung, x2 = "{template}"),
                            glue_id = list(x1 = "id", x2 = "{template}"),
                            glue_reactive = list(x1 = "call", x2 = lehrveranstaltung, x3 = "{template}"),
                            .data = .data,
                            .values = .values,
                            parent = self)
  }

  call_multiple_modules_2(module_templates = einstellungen,
                          glue_module = list(x1 = "einstellungen", x2 = "{template}"),
                          glue_id = list(x1 = "id_einstellungen", x2 = "{template}"),
                          glue_reactive = list(x1 = "call_einstellungen", x2 = "{template}"),
                          .data = .data,
                          .values = .values,
                          parent = self)

  call_menubar <- callModule(
    module = menubar,
    id = "id_menubar",
    .data = .data,
    .values = .values,
    parent = self
  )
}

shinyApp(ui, server)
