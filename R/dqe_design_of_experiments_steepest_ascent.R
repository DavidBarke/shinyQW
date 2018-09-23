#' @export
dqe_design_of_experiments_projekt_steepest_ascent_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      uiOutput(
        outputId = ns("select_data")
      ),
      uiOutput(
        outputId = ns("select_versuchsplan")
      ),
      uiOutput(
        outputId = ns("uebersicht_ui")
      ),
      uiOutput(
        outputId = ns("contour_ui")
      ),
      uiOutput(
        outputId = ns("erklaert")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabset_main_steepest_ascent"),
        tabPanel(
          title = "Übersicht",
          DT::dataTableOutput(
            outputId = ns("show_data")
          )
        ),
        tabPanel(
          title = "Kennzahlen",
          DT::dataTableOutput(
            outputId = ns("show_kennzahlen")
          )
        ),
        tabPanel(
          title = "Visualisierungen",
          tabsetPanel(
            id = ns("tabset_visualisierungen"),
            tabPanel(
              title = "Contour Plots",
              tabsetPanel(
                id = ns("tabset_contour_plots"),
                tabPanel(
                  title = "2D",
                  plotlyOutput(
                    outputId = ns("contour_plot_2D")
                  )
                ),
                tabPanel(
                  title = "3D",
                  plotlyOutput(
                    outputId = ns("contour_plot_3D")
                  )
                )
              )
            ),
            tabPanel(
              title = "SA-Versuche",
              plotlyOutput(
                outputId = ns("sa_versuche")
              )
            )
          )
        )
      )
    )
  )
}

#' @export
dqe_design_of_experiments_projekt_steepest_ascent <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                                                              parent, ...) {
  self <- node$new("steepest_ascent", parent, session)

  ns <- session$ns

  # Reactives

  data <- reactive({
    if (length_data_storage() != 0) {
      data_1 <- user_data_storage[[input$select_data]]
      return(data_1)
    }
  })

  data_versuchsplan <- reactive({
    if (length_data_storage() != 0) {
      data_1 <- user_data_storage[[input$select_versuchsplan]]
      return(data_1)
    }
  })

  data_versuchsplan_mapped <- reactive({
    data_1 <- data_versuchsplan()
    factor_names <- factor_names()
    max <- c()
    min <- c()
    j <- 1
    for (i in which(names(data_1) %in% factor_names)) {
      max[j] <- max(data_1[,i])
      min[j] <- min(data_1[,i])
      j <- j + 1
    }
    for (i in 1:length(factor_names)) {
      factor_name <- factor_names[i]
      j <- which(names(data_1) == factor_name)
      data_1[,j] <- map_range_1(data_1[,j], max[i], min[i])
    }
    return(data_1)
  })

  data_versuchsplan_mapped_ohne_centerpoints <- reactive({
    data_1 <- data_versuchsplan_mapped()
    factor_names <- factor_names()
    data_1 <- data_1[which(data_1[[factor_names[1]]] != 0),]
  })

  map_range_1 <- function(values, max, min) {
    diff <- max - min
    return((values - mean(c(max, min))) / (0.5 * diff))
  }

  data_mapped <- reactive({
    data_vp <- data_versuchsplan()
    data_sa <- data()
    factor_names <- factor_names()
    i_vp <- which(names(data_vp) %in% factor_names)
    max <- c()
    min <- c()
    j <- 1
    for (i in i_vp) {
      max[j] <- max(data_vp[,i])
      min[j] <- min(data_vp[,i])
      j <- j + 1
    }
    for (i in 1:length(factor_names)) {
      factor_name <- factor_names[i]
      i_sa <- which(names(data_sa) == factor_name)
      data_sa[,i_sa] <- map_range_1(data_sa[,i_sa], max[i], min[i])
    }
    return(data_sa)
  })

  length_data_storage <- reactive({
    return(length(names(user_data_storage)))
  })

  erklaert <- reactive({
    erklaert <- input$erklaert
    return(erklaert)
  })

  factor_names <- reactive({
    factor_names <- c("Fluegellaenge", "Koerperlaenge", "Einschnitt", "Papierstaerke")
    return(factor_names)
  })

  varied_factors <- reactive({
    data_1 <- data_mapped() %>%
      select(one_of(factor_names()))
    varied_factors <- c()
    j <- 1
    for (i in 1:length(data_1)) {
      if (sum(data_1[,i]) != 0) {
        varied_factors[j] <- names(data_1)[i]
        j <- j + 1
      }
    }
    return(varied_factors)
  })

  # Output

  output$select_data <- renderUI({
    selectInput(
      inputId = ns("select_data"),
      label = "Wähle Datensatz",
      choices = names(user_data_storage),
      selected = if ("Steepest Ascent" %in% names(user_data_storage)) {names(user_data_storage)[str_detect(names(user_data_storage), "Steepest Ascent")]}
    )
  })

  output$select_versuchsplan <- renderUI({
    selectInput(
      inputId = ns("select_versuchsplan"),
      label = "Wähle Datensatz",
      choices = names(user_data_storage),
      selected = if ("Versuchsplan" %in% names(user_data_storage)) {names(user_data_storage)[str_detect(names(user_data_storage), "Versuchsplan")]}
    )
  })

  output$uebersicht_ui <- renderUI({
    checkboxInput(
      inputId = ns("kodiert"),
      label = "Kodierte Werte",
      value = FALSE
    )
  })

  output$erklaert <- renderUI({
    tagList(
      selectInput(
        inputId = ns("erklaert"),
        label = "Zielgröße",
        choices = names(data()),
        selected = "Flugdauer"
      )
    )
  })

  # Struktur der Daten
  # Spalten: Wurf, Ort, Datum, Abwurfhoehe, Flugdauer, Papierstaerke

  # Übersicht

  output$show_data <- DT::renderDataTable({
    if (input$kodiert) {
      data_mapped()
    } else {
      data()
    }
  })

  # Kennzahlen für Orte

  output$show_kennzahlen <- DT::renderDataTable({
    if (length_data_storage() != 0) {
      data <- data() %>%
        group_by(Papierstaerke) %>%
        summarise(`Durchschnittliche Flugdauer` = mean(Flugdauer),
                  `Standardabweichung` = sqrt((n() - 1)/(n())) * sd(Flugdauer),
                  `cp-Wert` = 0.5 / (6 * sqrt((n() - 1) / (n())) * sd(Flugdauer)))
      data <- datatable(data)
    }
  })

  # Visualisierungen

  ## Contour Plots

  coef_lm_contour <- reactive({
    s <- summary(lm_contour())
    s <- s$coefficients[,1]
    return(s)
  })

  eff_1 <- reactive({
    s <- coef_lm_contour()
    eff_1 <- s[which(names(s) == input$contour_faktor_1)]
    return(eff_1)
  })

  eff_2 <- reactive({
    s <- coef_lm_contour()
    eff_2 <- s[which(names(s) == input$contour_faktor_2)]
    return(eff_2)
  })

  lm_contour <- reactive({
    vars <- varied_factors()
    if (input$wechselwirkung == TRUE) {
      formel <- paste(erklaert(), " ~ ", input$contour_faktor_1, " + ", input$contour_faktor_2, sep = "")
    } else {
      formel <- paste(erklaert(), " ~ ", input$contour_faktor_1, " + ", input$contour_faktor_2, sep = "")
    }
    data_1 <- data_versuchsplan_mapped_ohne_centerpoints()
    lm_2 <- lm(formel, data = data_1)
    return(lm_2)
  })

  lm_sa <- reactive({
    vars <- varied_factors()
    formel <- paste(erklaert(), " ~ ", sep = "")
    for (i in 1:length(vars)) {
      if (i == length(vars)) {
        formel <- paste(formel, vars[i], sep = "")
      } else {
        formel <- paste(formel, vars[i], " + ", sep = "")
      }
    }
    data_1 <- data_versuchsplan_mapped_ohne_centerpoints()
    lm_sa <- lm(formel, data = data_1)
    return(lm_sa)
  })

  ### 2D

  output$contour_plot_2D <- renderPlotly({
    if (length_data_storage() != 0) {
      vars <- varied_factors()
      data_1 <- data_mapped()
      lm_2 <- lm_contour()
      x_seq <- seq(-1, 1, by = 0.01)
      y_seq <- seq(-1, 1, by = 0.01)
      df_seq <- expand.grid(x_seq, y_seq)
      names(df_seq) <- c(input$contour_faktor_1, input$contour_faktor_2)
      df_seq$predict <- predict(lm_2, df_seq)
      plot <- plotly::plot_ly(data = df_seq, x = df_seq[[input$contour_faktor_1]], y = df_seq[[input$contour_faktor_2]], z = ~predict,
                      type = "contour",
                      colorbar = list(
                        thickness = 60,
                        tickmode = "array",
                        tickvals = c(100, 500)
                      )
      ) %>%
        plotly::add_trace(
          type = "scatter",
          mode = "lines",
          line = list(
            color = "black"
          ),
          x = c(0, eff_1()),
          y = c(0, eff_2())
        ) %>%
        plotly::add_trace(
          type = "scatter",
          mode = "lines",
          line = list(
            color = "black"
          ),
          data = data_1,
          x = data_1[[input$contour_faktor_1]],
          y = data_1[[input$contour_faktor_2]]
        ) %>%
        plotly::layout(
          xaxis = list(
            title = input$contour_faktor_1,
            tickmode = "array",
            tickvals = c(-1, 1)
          ),
          yaxis = list(
            title = input$contour_faktor_2,
            tickmode = "array",
            tickvals = c(-1, 1)
          )
        )
      return(plot)
    }
  })

  ### 3D

  output$contour_plot_3D <- renderPlotly({
    if (length_data_storage() != 0) {
      lm_2 <- lm_contour()
      x_seq <- seq(-1, 1, by = 0.05)
      y_seq <- seq(-1, 1, by = 0.05)
      df_seq <- expand.grid(x_seq, y_seq)
      names(df_seq) <- c(input$contour_faktor_1, input$contour_faktor_2)
      df_seq$predict <- predict(lm_2, df_seq)
      formel_2 <- paste(input$contour_faktor_2, " ~ ", input$contour_faktor_1, sep = "")
      m_lm <- reshape2::acast(df_seq, formula = formel_2, value.var = "predict")
      plot <- plotly::plot_ly(type = "surface", x = x_seq, y = y_seq, z = m_lm, colors = "Blues", name = "Aktivität") %>%
        plotly::layout(
          scene = list(
            xaxis = list(
              title = input$contour_faktor_1
            ),
            yaxis = list(
              title = input$contour_faktor_2
            ),
            zaxis = list(
              title = erklaert()
            ),
            camera = list(
              eye = list(
                x = 1*1.5,
                y = 0.75*1.5,
                z = 0.75*1.5
              )
            )
          )
        )
      return(plot)
    }
  })

  ### UI

  output$contour_ui <- renderUI({
    if (input$tabset_main_steepest_ascent == "Visualisierungen" && input$tabset_visualisierungen == "Contour Plots") {
      tagList(
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("contour_faktor_1"),
              label = "Wähle den Faktor für die x-Achse:",
              choices = c("Fluegellaenge", "Einschnitt", "Papierstaerke")
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = ns("contour_faktor_2"),
              label = "Wähle den Faktor für die y-Achse:",
              choices = c("Koerperlaenge", "Einschnitt", "Papierstaerke")
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            checkboxInput(
              inputId = ns("wechselwirkung"),
              label = "Betrachte Wechselwirkungen",
              value = TRUE
            )
          )
        ),
        verbatimTextOutput(
          outputId = ns("lm_contour_1")
        )
      )
    }
  })

  output$lm_contour_1 <- renderPrint({
    print(summary(lm_contour()))
  })

  observeEvent(input$contour_faktor_1, {
    selected <- input$contour_faktor_2
    updateSelectInput(session,
                      inputId = "contour_faktor_2",
                      choices = factor_names()[which(factor_names() != input$contour_faktor_1)],
                      selected = selected)
  })

  observeEvent(input$contour_faktor_2, {
    selected <- input$contour_faktor_1
    updateSelectInput(session,
                      inputId = "contour_faktor_1",
                      choices = factor_names()[which(factor_names() != input$contour_faktor_2)],
                      selected = selected)
  })

  ## SA-Versuche

  output$sa_versuche <- renderPlotly({
    data_1 <- data_mapped()
    # Vorhersage
    data_vorhersage <- select(data_1, one_of(factor_names()))
    data_vorhersage <- unique(data_vorhersage)
    data_vorhersage$Vorhersage <- predict(lm_sa(), data_vorhersage)
    # Beobachtung
    data_beobachtung <- data_1 %>%
      group_by_(varied_factors()[1]) %>%
      summarise(Beobachtung = mean(Flugdauer))
    data_2 <- full_join(data_vorhersage, data_beobachtung, by = varied_factors()[1]) %>%
      arrange_(varied_factors()[1]) %>%
      mutate(Wurf = 1:nrow(data_beobachtung))
    plot <- ggplot(data = data_2, mapping = aes(x = Wurf)) +
      geom_point(mapping = aes(y = Beobachtung), col = "blue") +
      geom_point(mapping = aes(y = Vorhersage), col = "red") +
      theme_bw()
    plot <- ggplotly(plot)
    return(plot)
  })
}
