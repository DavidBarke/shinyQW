library(shiny)
library(shinydashboard)
library(shinyAce)
library(shinyjs)
library(colourpicker)
library(shinyQW)
library(tidyverse)
library(plotly)
library(R6)
library(R.utils)

# SOURCE -----------------------------------------------------------------------
# Damit nicht nach jeder Veränderung shinyQW neu gebuilded werden muss
x <- sourceDirectory(path = "../../../R", encoding = "UTF-8")

# Globals ----------------------------------------------------------------------

viewer_box <- tabBox_R6$new(id = "viewer")

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
  title = "QW: Applied Data Science",
  skin = "black",
  dashboardHeader(
    title = "QW-App"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        "IDA",
        menuSubItem(
          "Import",
          "tab_import"
        ),
        menuSubItem(
          "Tidy",
          "tab_tidy"
        ),
        menuSubItem(
          "Transform",
          "tab_transform"
        ),
        menuSubItem(
          "Programming",
          "tab_programming"
        ),
        menuSubItem(
          "Regular Expressions",
          "tab_regular_expressions"
        ),
        menuSubItem(
          "Visualize",
          "tab_visualize"
        )
      ),
      menuItem(
        "DQE",
        menuSubItem(
          "Deskriptive Statistik",
          "tab_deskriptive_statistik"
        ),
        menuSubItem(
          "Wahrscheinlichkeitsrechnung",
          "tab_wahrscheinlichkeitsrechnung"
        ),
        menuSubItem(
          "Verteilungsmodelle",
          "tab_verteilungsmodelle"
        ),
        menuSubItem(
          "Statistische Prozesskontrolle",
          "tab_statistische_prozesskontrolle"
        ),
        menuSubItem(
          "Modellstrukturen",
          "tab_modellstrukturen"
        ),
        menuSubItem(
          "Design of Experiments",
          "tab_design_of_experiments"
        )
      ),
      menuItem(
        "Einstellungen",
        menuSubItem(
          "Allgemein",
          "tab_allgemein"
        ),
        menuSubItem(
          "ggplot2",
          "tab_ggplot2"
        ),
        menuSubItem(
          "plotly",
          "tab_plotly"
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
    actionButton(
      inputId = "append",
      label = "Append Tab"
    ),
    viewer_box$tabBox(),
    tabItems(
      tabItem(
        tabName = "tab_import",
        ida_import_ui(
          id = "id_import"
        )
      ),
      tabItem(
        tabName = "tab_tidy",
        ida_tidy_ui(
          id = "id_tidy"
        )
      ),
      tabItem(
        tabName = "tab_transform",
        ida_transform_ui(
          id = "id_transform"
        )
      ),
      tabItem(
        tabName = "tab_programming",
        ida_programming_ui(
          id = "id_programming"
        )
      ),
      tabItem(
        tabName = "tab_regular_expressions",
        ida_regular_expressions_ui(
          id = "id_regular_expressions"
        )
      ),
      tabItem(
        tabName = "tab_visualize",
        ida_visualize_ui(
          id = "id_visualize"
        )
      ),
      tabItem(
        tabName = "tab_design_of_experiments",
        dqe_design_of_experiments_ui(
          id = "id_design_of_experiments"
        )
      ),
      tabItem(
        tabName = "tab_deskriptive_statistik",
        dqe_deskriptive_statistik_ui(
          id = "id_deskriptive_statistik"
        )
      ),
      tabItem(
        tabName = "tab_modellstrukturen",
        dqe_modellstrukturen_ui(
          id = "id_modellstrukturen"
        )
      ),
      tabItem(
        tabName = "tab_statistische_prozesskontrolle",
        dqe_statistische_prozesskontrolle_ui(
          id = "id_statistische_prozesskontrolle"
        )
      ),
      tabItem(
        tabName = "tab_testtheorie",
        dqe_testtheorie_ui(
          id = "id_testtheorie"
        )
      ),
      tabItem(
        tabName = "tab_verteilungsmodelle",
        dqe_verteilungsmodelle_box(
          id = "id_verteilungsmodelle"
        )
      ),
      tabItem(
        tabName = "tab_wahrscheinlichkeitsrechnung",
        dqe_wahrscheinlichkeitsrechnung_ui(
          id = "id_wahrscheinlichkeitsrechnung"
        )
      ),
      tabItem(
        tabName = "tab_allgemein",
        einstellungen_allgemein_ui(
          id = "id_allgemein"
        )
      ),
      tabItem(
        tabName = "tab_ggplot2",
        einstellungen_ggplot2_ui(
          id = "id_ggplot2"
        )
      ),
      tabItem(
        tabName = "tab_plotly",
        einstellungen_plotly_ui(
          id = "id_plotly"
        )
      )
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  self <- node$new("app", session = session)

  viewer_box$set_session(session)

# REACTIVE VALUES --------------------------------------------------------------

  user_data_storage <- reactiveValues()

  permanent_data_storage <- reactiveValues(
    mtcars = mtcars,
    iris = iris
  )

  values <- reactiveValues(session_tree = self)


  for (lehrveranstaltung in names(lehrveranstaltungen)) {
    themen <- lehrveranstaltungen[[lehrveranstaltung]]
    call_multiple_modules_2(module_templates = themen,
                            glue_module = list(x1 = lehrveranstaltung, x2 = "{template}"),
                            glue_id = list(x1 = "id", x2 = "{template}"),
                            glue_reactive = list(x1 = "call", x2 = lehrveranstaltung, x3 = "{template}"),
                            data = list(
                              user_data_storage = user_data_storage,
                              permanent_data_storage = permanent_data_storage
                            ),
                            values = values,
                            parent = self)
  }

  call_multiple_modules_2(module_templates = einstellungen,
                          glue_module = list(x1 = "einstellungen", x2 = "{template}"),
                          glue_id = list(x1 = "id_einstellungen", x2 = "{template}"),
                          glue_reactive = list(x1 = "call_einstellungen", x2 = "{template}"),
                          data = list(
                            user_data_storage = user_data_storage,
                            permanent_data_storage = permanent_data_storage
                          ),
                          values = values,
                          parent = self)

  observeEvent(input$append, {
    viewer_box$appendTab(
      tabPanel("Appended"),
      select = TRUE
    )
  })
}

shinyApp(ui, server)
