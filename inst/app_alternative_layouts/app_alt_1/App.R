library(shiny)
library(shinydashboard)
library(shinyAce)
library(shinyjs)
library(colourpicker)
library(shinyQW)
library(tidyverse)
library(plotly)

full_box <- function(..., title = NULL, footer = NULL, status = NULL,
                     solidHeader = FALSE, background = NULL, width = 6,
                     height = NULL, collapsible = FALSE, collapsed = FALSE) {
  h <- box(..., title = title, footer = footer, status = status,
           solidHeader = solidHeader, background = background, width = width,
           height = height, collapsible = collapsible, collapsed = collapsed)
  h$attribs$style <- "padding: 0px"
  h
}

full_dashboardBody <- function(...) {
  h <- dashboardBody(...)
  h$children[[1]]$attribs$style <- "padding: 0px"
  h
}

# Globals ----------------------------------------------------------------------

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
          tabName = "tab_import"
        ),
        menuSubItem(
          "Tidy",
          tabName = "tab_tidy"
        ),
        menuSubItem(
          "Transform",
          tabName = "tab_transform"
        )
      ),
      menuItem(
        "DQE",
        menuSubItem(
          "Deskriptive Statistik",
          tabName = "tab_deskriptive_statistik"
        ),
        menuSubItem(
          "Verteilungsmodelle",
          tabName = "tab_verteilungsmodelle"
        )
      ),
      menuItem(
        "Einstellungen",
        menuSubItem(
          "Allgemein",
          tabName = "tab_allgemein"
        ),
        menuSubItem(
          "ggplot2",
          tabName = "tab_ggplot2"
        ),
        menuSubItem(
          "plotly",
          tabName = "tab_plotly"
        )
      )
    )
  ),
  full_dashboardBody(
    full_box(
      title = "Menuleiste",
      collapsible = TRUE,
      width = 12,
      solidHeader = TRUE,
      box("Box_1"),
      box("Box_2")
    ),
    tabItems(
      tabItem(
        tabName = "tab_import"
      ),
      tabItem(
        tabName = "tab_tidy"
      ),
      tabItem(
        tabName = "tab_transform"
      ),
      tabItem(
        tabName = "tab_deskriptive_statistik"
      ),
      tabItem(
        tabName = "tab_verteilungsmodelle"
      ),
      tabItem(
        tabName = "tab_allgemein",
        shinyQW::einstellungen_allgemein_ui(
          id = "id_allgemein"
        )
      ),
      tabItem(
        tabName = "tab_ggplot2",
        shinyQW::einstellungen_ggplot2_ui(
          id = "id_ggplot2"
        )
      ),
      tabItem(
        tabName = "tab_plotly"
      )
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  self <- node$new("app", session = session)

# REACTIVE VALUES --------------------------------------------------------------

  user_data_storage <- reactiveValues()

  permanent_data_storage <- reactiveValues(
    mtcars = mtcars,
    iris = iris
  )

  values <- reactiveValues(session_tree = self)


  for (lehrveranstaltung in names(lehrveranstaltungen)) {
    themen <- lehrveranstaltungen[[lehrveranstaltung]]
    shinyQW::call_multiple_modules_2(module_templates = themen,
                            glue_module = list(x1 = lehrveranstaltung, x2 = "{template}"),
                            glue_id = list(x1 = "id", x2 = lehrveranstaltung, x3 = "{template}"),
                            glue_reactive = list(x1 = "call", x2 = lehrveranstaltung, x3 = "{template}"),
                            user_data_storage = user_data_storage,
                            permanent_data_storage = permanent_data_storage,
                            values = values,
                            parent = self)
  }

  shinyQW::call_multiple_modules_2(module_templates = einstellungen,
                          glue_module = list(x1 = "einstellungen", x2 = "{template}"),
                          glue_id = list(x1 = "id_einstellungen", x2 = "{template}"),
                          glue_reactive = list(x1 = "call_einstellungen", x2 = "{template}"),
                          user_data_storage = user_data_storage,
                          permanent_data_storage = permanent_data_storage,
                          values = values,
                          parent = self)
}

shinyApp(ui, server)
