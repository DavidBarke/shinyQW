# TODO: values überarbeiten, observeEvents sind überflüssig, gleiches gilt für
# die observeEvents vom Import-Modul

# PACKAGES ---------------------------------------------------------------------
if (!require(githubinstall)) {
  install.packages("githubinstall")
  require(githubinstall)
}

if (!require(shiny)) {
  install.packages("shiny")
  require(shiny)
}

if (!require(rmarkdown)) {
  install.packages("rmarkdown")
  require(rmarkdown)
}

if (!require(digest)) {
  install.packages("digest")
  require(digest)
}

if (!require(shinyjs)) {
  githubinstall("shinyjs")
  require(shinyjs)
}

if (!require(shinyAce)) {
  githubinstall("shinyAce")
  require(shinyAce)
}

if (!require(shinythemes)) {
  install.packages("shinythemes")
  require(shinythemes)
}

if (!require(shinydashboard)) {
  install.packages("shinydashboard")
  require(shinydashboard)
}

if (!require(colourpicker)) {
  install.packages("colourpicker")
  require(colourpicker)
}

if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

if (!require(DT)) {
  install.packages("DT")
  require(DT)
}

if (!require(plotly)) {
  install.packages("plotly")
  require(plotly)
}

if (!require(R6)) {
  install.packages("R6")
  require(R6)
}

if (!require(shinyQW)) {
  install.packages("shinyQW")
  require(shinyQW)
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

# UI ---------------------------------------------------------------------------
# Für alle Lehrveranstaltungen wird jeweils ein navbarMenu erstellt, dass die
# einzelnen Theme in tabPaneln enthält
for (lehrveranstaltung in names(lehrveranstaltungen)) {
  assign(lehrveranstaltung %_% "navbarMenu_args", list(
    title = toupper(lehrveranstaltung)
  ))
  args_list <- list()
  for (thema in lehrveranstaltungen[[lehrveranstaltung]]) {
    args_list[[length(args_list) + 1]] <- tabPanel(
      title = firstup(str_replace_all(thema, "_", " ")),
      do.call(
        lehrveranstaltung %_% thema %_% "ui", list(id = "id" %_% lehrveranstaltung %_% thema)
      )
    )
  }
  assign(lehrveranstaltung %_% "navbarMenu_args",
         c(get(lehrveranstaltung %_% "navbarMenu_args"), unname(args_list))
  )
}

ui <- tagList(
  useShinyjs(),
  # CSS
  tags$head(
    tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "styles.css"
    )
  ),
  actionButton(
    inputId = "print_session_tree",
    label = "Print Session Tree"
  ),
  navbarPage(
    theme = shinytheme("lumen"),
    title = "QW: Applied Data Science",
    tabPanel(
      title = "Import",
      tab_import_ui(id = "id_import")
    ),
    do.call(navbarMenu, ida_navbarMenu_args),
    do.call(navbarMenu, dqe_navbarMenu_args),
    navbarMenu(
      title = "DRE"
    ),
    navbarMenu(
      title = "Einstellungen",
      tabPanel(
        title = "Allgemein",
        einstellungen_allgemein_ui(
          id = "id_einstellungen_allgemein"
        )
      ),
      tabPanel(
        title = "DQE",
        einstellungen_dqe_ui(
          id = "id_einstellungen_dqe"
        )
      ),
      tabPanel(
        title = "ggplot2",
        einstellungen_ggplot2_ui(
          id = "id_einstellungen_ggplot2"
        )
      ),
      tabPanel(
        title = "plotly",
        einstellungen_plotly_ui(
          id = "id_einstellungen_plotly"
        )
      )
    )
  )
)

server <- function(input, output, session) {

  self <- node$new("app", session = session)

  user_data_storage <- reactiveValues()
  permanent_data_storage <- reactiveValues(mtcars = mtcars, diamonds = diamonds, iris = iris)
  values <- reactiveValues(session_tree = self)

  # Strukturmodule -----------------------------------------------------------------------------
  rvs <- reactiveValues()

  for (lehrveranstaltung in names(lehrveranstaltungen)) {
    themen <- lehrveranstaltungen[[lehrveranstaltung]]
    call_multiple_modules_2(module_templates = themen,
                            glue_module = list(x1 = lehrveranstaltung, x2 = "{template}"),
                            glue_id = list(x1 = "id", x2 = lehrveranstaltung, x3 = "{template}"),
                            glue_reactive = list(x1 = "call", x2 = lehrveranstaltung, x3 = "{template}"),
                            user_data_storage = user_data_storage,
                            permanent_data_storage = permanent_data_storage,
                            values = values,
                            parent = self)
  }

  ## Einstellungen
  einstellungen <- c("allgemein", "ggplot2", "plotly", "dqe")

  call_multiple_modules_2(module_templates = einstellungen,
                          glue_module = list(x1 = "einstellungen", x2 = "{template}"),
                          glue_id = list(x1 = "id_einstellungen", x2 = "{template}"),
                          glue_reactive = list(x1 = "call_einstellungen", x2 = "{template}"),
                          user_data_storage = user_data_storage,
                          permanent_data_storage = permanent_data_storage,
                          values = values,
                          parent = self)

  # Funktionsmodule ------------------------------------------------------------
  # Aufrufen der Module für alle Module
  call_import <- callModule(module = tab_import,
                            id = "id_import",
                            user_data_storage = user_data_storage,
                            permanent_data_storage = permanent_data_storage,
                            values = values,
                            parent = self)

  # Observer -------------------------------------------------------------------
  ## Befüllen von user_data_storage durch Import
  observeEvent(call_import(), {
    import <- call_import()
    import_user_data_storage <- import$user_data_storage
    req(import_user_data_storage)
    for (i in 1:length(names(import_user_data_storage))) {
      name <- names(import_user_data_storage)[[i]]
      user_data_storage[[name]] <- import_user_data_storage[[i]]
    }
  })

  ## Befüllen von values$einstellungen
  # TODO: KANN IM MODUL GEMACHT WERDEN (Referenzsemantik von reactiveValues)
  observeEvent(call_einstellungen_allgemein(), {
    values$einstellungen$allgemein <- call_einstellungen_allgemein()$values$allgemein
  })

  observeEvent(call_einstellungen_ggplot2(), {
    values$einstellungen$ggplot2 <- call_einstellungen_ggplot2()$values$ggplot2
  })

  observeEvent(call_einstellungen_plotly(), {
    values$einstellungen$plotly <- call_einstellungen_plotly()$values$plotly
  })

  observeEvent(call_einstellungen_dqe(), {
    values$einstellungen$dqe <- call_einstellungen_dqe()$values$dqe
  })




  # TEST

  observeEvent(input$print_session_tree, {
    l <- values$session_tree$create_list()
    str(l)
  })

}

shinyApp(ui, server)
