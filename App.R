library(shiny)
library(shinydashboard)
library(shinyAce)
library(shinyjs)
library(shinyjqui)
library(colourpicker)
#library(shinyQW)
library(tidyverse)
library(plotly)
library(R6)
library(R.utils)
library(DT)
library(patchwork)
library(readr)
library(data.table)
library(listviewer)
library(qcc)
data("pistonrings")

# SOURCE -----------------------------------------------------------------------
# Es ist wichtig, dass modifiedOnly = FALSE ist!
sourceDirectory(path = "www/modules", encoding = "UTF-8", modifiedOnly = FALSE)

# Globals ----------------------------------------------------------------------

.language = "de"
options(DT.options = list(scrollX = TRUE))

tabList <- tabList_R6$new(id = "placeholder", sortable = TRUE)

content_list <- content_list_R6$new(id = "content", sortable = TRUE)

viewer_data <- tabBox_R6$new(
  id = "viewer_data",
  title = label_lang(
    de = "Daten",
    en = "Data"
  ),
  width = 12
)

viewer_plot <- tabBox_R6$new(
  id = "viewer_plot",
  title = "Plots",
  width = 12
)

lehrveranstaltungen <- list(
  ida = c(
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
        inputId = "tab_data_btn",
        label = label_lang(
          de = "Daten",
          en = "Data"
        )
      ),
      actionItem(
        inputId = "tab_import_btn",
        label = "Import"
      ),
      actionItem(
        inputId = "ida_btn",
        label = "IDA",
        actionSubItem(
          "tab_tidy_btn",
          "Tidy"
        ),
        actionSubItem(
          "tab_transform_btn",
          label_lang(
            de = "Transformieren",
            en = "Transform"
          )
        ),
        actionSubItem(
          "tab_programming_btn",
          label_lang(
            de = "Programmieren",
            en = "Programming"
          )
        ),
        actionSubItem(
          "tab_regular_expressions_btn",
          "Regular Expressions"
        ),
        actionSubItem(
          "tab_visualize_btn",
          label_lang(
            de = "Visualisieren",
            en = "Visualize"
          )
        )
      ),
      actionItem(
        "dqe_btn",
        "DQE",
        actionSubItem(
          "tab_deskriptive_statistik_btn",
          label_lang(
            de = "Deskriptive Statistik",
            en = "Descriptive Statistics"
          )
        ),
        actionSubItem(
          "tab_wahrscheinlichkeitsrechnung_btn",
          label_lang(
            de = "Wahrscheinlichkeitsrechnung",
            en = "Probabilities"
          )
        ),
        actionSubItem(
          "tab_verteilungsmodelle_btn",
          label_lang(
            de = "Verteilungsmodelle",
            en = "Distributions"
          )
        ),
        actionSubItem(
          "tab_statistische_prozesskontrolle_btn",
          label_lang(
            de = "Statistische Prozesskontrolle",
            en = "Statistical Process Control"
          )
        ),
        actionSubItem(
          "tab_modellstrukturen_btn",
          label_lang(
            de = "Modellstrukturen",
            en = "Modelling"
          )
        ),
        actionSubItem(
          "tab_design_of_experiments_btn",
          "Design of Experiments"
        )
      ),
      actionItem(
        "einstellungen_btn",
        label_lang(
          de = "Einstellungen",
          en = "Settings"
        ),
        actionSubItem(
          "tab_allgemein_btn",
          label_lang(
            de = "Allgemein",
            en = "General"
          )
        ),
        actionSubItem(
          "tab_ggplot2_btn",
          "ggplot2"
        ),
        actionSubItem(
          "tab_plotly_btn",
          "plotly"
        )
      ),
      actionItem(
        inputId = "development_btn",
        label = label_lang(
          de = "Entwicklung",
          en = "Development"
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
    fluidRow(
      column(
        width  = 6,
        tabList$container(),
        content_list$container()
      ),
      column(
        width = 6,
        viewer_data$tabBox(collapsible = TRUE),
        viewer_plot$tabBox(collapsible = TRUE)
      )
    ),
    actionButton(
      "show_show",
      label = "Show"
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  self <- node$new("app", session = session)
  
  observeEvent(input$show_show, {
    show(
      selector = paste("#element_container_tab_deskriptive_statistik_element")
    )
  })

# SET SESSION ------------------------------------------------------------------

  tabList$set_session(session)
  content_list$set_session(session)

  viewer_data$set_session(session)
  viewer_plot$set_session(session)

# REACTIVE VALUES --------------------------------------------------------------

  .data <- data_R6$new()

  .values <- list(
    session = reactiveValues(
      tree = self,
      # COMPROMISE SOLUTION, NEEDED FOR shinyjs::show IN NESTED MODULES
      app = session
    ),
    viewer = reactiveValues(
      data = viewer_data,
      plot = viewer_plot,
      content = content_list
    ),
    einstellungen = list(
      allgemein = reactiveValues(),
      ggplot2 = reactiveValues(),
      plotly = reactiveValues(),
      dqe = reactiveValues()
    ),
    .language = .language,
    .data = reactiveValues(
      groups = .data$get_group_names()
    )
  )

# CONNECT TABLIST WITH ACTIONBUTTONS

  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_deskriptive_statistik_element",
      title = label_lang(
        de = "Deskriptive Statistik",
        en = "Descriptive Statistics"
      ),
      tabPanel_list = dqe_deskriptive_statistik_tabPanel(
        id = "id_deskriptive_statistik"
      )
    ),
    actionButton_id = "tab_deskriptive_statistik_btn",
    actionButton_session = session
  )

  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_design_of_experiments_element",
      title = "Design of Experiments",
      tabPanel_list = dqe_design_of_experiments_tabPanel(
        id = "id_design_of_experiments"
      )
    ),
    actionButton_id = "tab_design_of_experiments_btn",
    actionButton_session = session
  )

  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_ggplot2_element",
      title = label_lang(
        de = "Einstellungen ggplot2",
        en = "Settings ggplot2"
      ),
      tabPanel_list = einstellungen_ggplot2_tabPanel(
        id = "id_einstellungen_ggplot2"
      )
    ),
    actionButton_id = "tab_ggplot2_btn",
    actionButton_session = session
  )

  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_verteilungsmodelle_element",
      title = label_lang(
        de = "Verteilungsmodelle",
        en = "Distributions"
      ),
      tabPanel_list = dqe_verteilungsmodelle_tabPanel(
        id = "id_verteilungsmodelle"
      )
    ),
    actionButton_id = "tab_verteilungsmodelle_btn",
    actionButton_session = session
  )
  
  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_wahrscheinlichkeitsrechnung_element",
      title = label_lang(
        de = "Wahrscheinlichkeitsrechnung",
        en = "Probability theory"
      ),
      tabPanel_list = dqe_wahrscheinlichkeitsrechnung_tabPanel(
        id = "id_wahrscheinlichkeitsrechnung"
      )
    ),
    actionButton_id = "tab_wahrscheinlichkeitsrechnung_btn",
    actionButton_session = session
  )

  content_list$add_element_actionButton(
    content_element = content_dialog_R6$new(
      id = "tab_import_element",
      title = "Import",
      tabPanel(
        title = label_lang(
          de = "Datenauswahl",
          en = "File Input"
        ),
        value = "file_input",
        import_ui(
          id = "id_import",
          .language = .language
        )
      ),
      tabPanel(
        title = label_lang(
          de = "Einstellungen",
          en = "Settings"
        ),
        value = "settings",
        import_ui_specific(
          id = "id_import",
          .language = .language
        )
      ),
      tabPanel(
        title = label_lang(
          de = "Datenimport",
          en = "Import"
        ),
        value = "import"
      ),
      footer = import_ui_preview(
        id = "id_import",
        .language = .language
      )
    ),
    actionButton_id = "tab_import_btn",
    actionButton_session = session
  )

  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_data_element",
      title = label_lang(
        de = "Daten",
        en = "Data"
      ),
      tabPanel_list = module_data_tabPanel(
        id = "id_data",
        .language = .language
      )
    ),
    actionButton_id = "tab_data_btn",
    actionButton_session = session
  )
  
  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_development_element",
      title = label_lang(
        de = "Entwicklung",
        en = "Development"
      ),
      tabPanel_list = development_tabPanel(
        id = "id_development"
      )
    ),
    actionButton_id = "development_btn",
    actionButton_session = session
  )
  
  content_list$add_element_actionButton(
    content_element = content_tabBox(
      id = "tab_statistische_prozesskontrolle",
      title = label_lang(
        de = "Statistische Prozesskontrolle",
        en = "Statistical process control"
      ),
      tabPanel_list = dqe_statistische_prozesskontrolle_tabPanel(
        id = "id_statistische_prozesskontrolle"
      )
    ),
    actionButton_id = "tab_statistische_prozesskontrolle_btn",
    actionButton_session = session
  )

# CALL MODULE ------------------------------------------------------------

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

  call_development <- callModule(
    module = development,
    id = "id_development",
    .data = .data,
    .values = .values,
    parent = self
  )

  call_import <- callModule(
    module = import,
    id = "id_import",
    .data = .data,
    .values = .values,
    parent = self
  )

  call_data <- callModule(
    module = module_data,
    id = "id_data",
    .data = .data,
    .values = .values,
    parent = self
  )
}

shinyApp(ui, server)
