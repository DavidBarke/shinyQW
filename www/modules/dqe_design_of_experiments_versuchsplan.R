#' @export
dqe_design_of_experiments_projekt_versuchsplan_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      uiOutput(
        outputId = ns("select_data")
      ),
      uiOutput(
        outputId = ns("select_factors")
      ),
      uiOutput(
        outputId = ns("interaction_ui")
      ),
      uiOutput(
        outputId = ns("pareto_ui")
      ),
      uiOutput(
        outputId = ns("contour_ui")
      ),
      uiOutput(
        outputId = ns("uebersicht_ui")
      ),
      textInput(
        inputId = ns("toleranzbreite"),
        label = "Toleranzbreite in s:",
        value = 0.5
      ),
      uiOutput(
        outputId = ns("erklaert")
      )
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tabset_main_versuchsplan"),
        tabPanel(
          title = "David"
        ),
        tabPanel(
          title = "Übersicht",
          DT::dataTableOutput(
            outputId = ns("show_data")
          )
        ),
        tabPanel(
          title = "Zusammenfassungen",
          tabsetPanel(
            id = ns("tabset_kennzahlen"),
            tabPanel(
              title = "Kennzahlen",
              DT::dataTableOutput(
                outputId = ns("show_kennzahlen")
              )
            ),
            tabPanel(
              title = "Lineares Modell",
              fluidRow(
                column(
                  width = 6,
                  verbatimTextOutput(
                    outputId = ns("summary_linear_model")
                  )
                ),
                column(
                  width = 6,
                  DT::dataTableOutput(
                    outputId = ns("zuordnung_1")
                  )
                )
              ),
              textOutput(
                outputId = ns("data_3")
              )
            )
          )
        ),
        tabPanel(
          title = "Visualisierungen",
          tabsetPanel(
            id = ns("tabset_visualisierungen"),
            tabPanel(
              title = "Histogramm",
              plotlyOutput(
                outputId = ns("histogramm")
              )
            ),
            tabPanel(
              title = "Effect Plot",
              plotlyOutput(
                outputId = ns("effect_plot")
              ),
              DT::dataTableOutput(
                outputId = ns("faktorstufen")
              )
            ),
            tabPanel(
              title = "Interaction Plot",
              fluidRow(
                plotlyOutput(
                  outputId = ns("interaction_plot")
                )
              )
            ),
            tabPanel(
              title = "Pareto Plot",
              plotlyOutput(
                outputId = ns("pareto_plot")
              ),
              DT::dataTableOutput(
                outputId = ns("zuordnung_2")
              )
            ),
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
              title = "Residuenanalyse",
              tabsetPanel(
                id = ns("tabset_residuen"),
                tabPanel(
                  title = "Residuen des linearen Modells",
                  plotlyOutput(
                    outputId = ns("residual_plot")
                  )
                ),
                tabPanel(
                  title = "Normal Q-Q Plot",
                  plotlyOutput(
                    outputId = ns("normal_qq_plot")
                  )
                ),
                tabPanel(
                  title = "Residuen der Centerpoints",
                  plotlyOutput(
                    outputId = ns("residual_centerpoints")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
dqe_design_of_experiments_projekt_versuchsplan <- function(
  input, output, session, .data, .values, parent, ...
) {

  user_data_storage <- .data$user_data_storage

  self <- node$new("versuchsplan", parent, session)

  ns <- session$ns

  # Reactives

  data <- reactive({
    if (length_data_storage() != 0) {
      data <- user_data_storage[[input$select_data]]
    }
  })

  data_ohne_centerpoints <- reactive({
    data_1 <- data()
    i <- which(names(data_1) %in% factor_names())[1]
    max <- max(data_1[,i])
    min <- min(data_1[,i])
    mean <- mean(c(max, min))
    data_1 <- data_1[which(data_1[,i] != mean),]
  })

  map_range_1 <- function(values, max, min) {
    diff <- max - min
    return((values - mean(c(max, min))) / (0.5 * diff))
  }

  data_mapped <- reactive({
    data_1 <- data()
    for (i in which(names(data_1) %in% factor_names())) {
      max <- max(data_1[,i])
      min <- min(data_1[,i])
      data_1[,i] <- map_range_1(data_1[,i], max, min)
    }
    return(data_1)
  })

  data_transform <- reactive({
    data_1 <- data_mapped()
    i <- which(names(data_1) %in% factor_names())[1]
    data_1 <- data_1[-which(data_1[,i] == 0),]
    return(data_1)
  })

  data_centerpoints <- reactive({
    data_1 <- data_mapped()
    i <- which(names(data_1) %in% factor_names())[1]
    data_1 <- data_1[which(data_1[,i] == 0),]
    return(data_1)
  })

  length_data_storage <- reactive({
    return(length(names(user_data_storage)))
  })

  length_select_factors <- reactive({
    return(length(input$select_factors))
  })

  erklaert <- reactive({
    erklaert <- input$erklaert
    return(erklaert)
  })

  lm_1 <- reactive({
    erklaert <- erklaert()
    factors <- factors()
    selected <- filter(factors, Faktor %in% input$select_factors)
    factors_letters <- selected$Label
    data_3 <- filter_all(data_transform(), all_vars(. != 0)) %>%
      dplyr::select(one_of(c(input$select_factors, erklaert)))
    anzahl <- max(2, length(factors_letters))
    formel <- as.formula(paste(erklaert, " ~ . ^ ", anzahl, sep = ""))
    lm_1 <- lm(formel, data = data_3)
    lm_1$call <- formel
    return(lm_1)
  })

  lm_coefficients <- reactive({
    lm_coefficients <- lm_1()$coefficients
    return(lm_coefficients)
  })

  factors <- reactive({
    factors <- c("Fluegellaenge", "Koerperlaenge", "Einschnitt", "Papierstaerke")
    letters <- as.character(LETTERS[1:length(factors)])
    data <- data.frame(Faktor = factors, Label = letters)
    data$Label <- as.character(data$Label)
    return(data)
  })

  factor_names <- reactive({
    factor_names <- c("Fluegellaenge", "Koerperlaenge", "Einschnitt", "Papierstaerke")
    return(factor_names)
  })

  selected_factors <- reactive({
    if (length_data_storage() != 0 && length_select_factors() != 0) {
      data <- factors()
      which <- which(data$Faktor %in% input$select_factors)
      data <- data[which,]
      data$Label <- as.character(data$Label)
      return(data)
    }
  })

  # Output

  output$select_data <- renderUI({
    selectInput(
      inputId = ns("select_data"),
      label = "Wähle Datensatz",
      choices = names(user_data_storage),
      selected = if ("Versuchsplan" %in% names(user_data_storage)) {names(user_data_storage)[str_detect(names(user_data_storage), "Versuchsplan")]}
    )
  })

  output$select_factors <- renderUI({
    if (input$tabset_main_versuchsplan == "Kennzahlen" || input$tabset_visualisierungen != "Interaction Plot" && input$tabset_visualisierungen != "Contour Plots") {
      factors <- c("Fluegellaenge", "Koerperlaenge", "Einschnitt", "Papierstaerke")
      selectizeInput(
        inputId = ns("select_factors"),
        label = "Wähle zu berücksichtigende Faktoren:",
        choices = factors,
        selected = factors,
        multiple = TRUE
      )
    }
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

  output$zuordnung_1 <- DT::renderDataTable({
    datatable(selected_factors())
  })

  output$zuordnung_2 <- DT::renderDataTable({
    datatable(selected_factors())
  })

  # Struktur der Daten
  # Spalten: Wurf, Ort, Datum, Abwurfhoehe, Flugdauer, Fluegellaenge, Koerperlaenge, Einschnitt, Papierart

  # Übersicht

  output$show_data <- DT::renderDataTable({
    if (input$include_centerpoints) {
      if (input$kodierte_werte) {
        return(data_mapped())
      } else {
        return(data())
      }
    } else {
      if (input$kodierte_werte) {
        return(data_transform())
      } else {
        return(data_ohne_centerpoints())
      }
    }
  })

  output$uebersicht_ui <- renderUI({
    if (input$tabset_main_versuchsplan == "Übersicht") {
      tagList(
        checkboxInput(
          inputId = ns("kodierte_werte"),
          label = "Kodierte Werte",
          value = FALSE
        ),
        checkboxInput(
          inputId = ns("include_centerpoints"),
          label = "Mit Centerpoints",
          value = TRUE
        )
      )
    }
  })

  # Zusammenfassungen

  ## Kennzahlen

  output$show_kennzahlen <- DT::renderDataTable({
    if (length_data_storage() != 0) {
      data <- data() %>%
        group_by(Ort) %>%
        summarise(`Durchschnittliche Flugdauer` = mean(Flugdauer),
                  `Standardabweichung` = sqrt((n() - 1)/(n())) * sd(Flugdauer),
                  `cp-Wert` = 0.5 / (6 * sqrt((n() - 1) / (n())) * sd(Flugdauer)))
      data <- datatable(data)
    }
  })

  ## Lineares Modell

  output$summary_linear_model <- renderPrint({
    if (length_data_storage() != 0) {
      if (!is.null(input$select_factors)) {
        lm_1 <- lm_1()
        s <- summary(lm_1)
        factors_df <- factors()
        effect_names <- row.names(s$coefficients)
        for (i in 1:length(factors_df$Faktor)) {
          effect_names <- str_replace_all(effect_names, factors_df$Faktor[i], factors_df$Label[i])
        }
        row.names(s$coefficients) <- effect_names
        print(s)
      }
    }
  })

  output$vereinfachtes_modell <- renderUI({
    checkboxInput(
      inputId = ns("vereinfachtes_modell"),
      label = "Vereinfachtes Modell"
    )
  })

  # Visualisierungen

  ## Histogramm

  output$histogramm <- renderPlotly({
    toleranzbreite <- as.numeric(input$toleranzbreite)
    flugdauer <- data()$Flugdauer
    k <- floor(sqrt(length(flugdauer)))
    mean_flugdauer <- mean(flugdauer)
    sd_flugdauer <- sd(flugdauer)
    min <- min(flugdauer, na.rm = TRUE)
    max <- max(flugdauer, na.rm = TRUE)
    R <- max - min
    b <- R / k
    breaks <- min + 0:k * b
    data_norm <- data.frame(x = seq(min, max, length.out = 100)) %>%
      mutate(y = dnorm(x = x, mean = mean_flugdauer, sd = sd_flugdauer))
    plot <- ggplot(data = data.frame(Flugdauer = flugdauer)) +
      geom_histogram(mapping = aes(x = Flugdauer, y = ..density..), breaks = breaks, closed = "left", col = "mediumblue", fill = "lightblue") +
      geom_vline(xintercept = mean_flugdauer - toleranzbreite/2, col = "red", linetype = "dashed") +
      geom_vline(xintercept = mean_flugdauer + toleranzbreite/2, col = "red", linetype = "dashed") +
      geom_line(data = data_norm, mapping = aes(x = x, y = y)) +
      labs(x = "Flugdauer", y = "Relative Häufigkeitsdichte") +
      theme_bw()
    plot <- ggplotly(plot)
    return(plot)
  })

  ## Effect Plot

  output$effect_plot <- renderPlotly({
    if (length_data_storage() != 0 && length_select_factors() != 0) {
      factors <- input$select_factors
      data <- data_transform() %>%
        dplyr::select(one_of(factors), Flugdauer) %>%
        dplyr::filter_all(all_vars(. != 0))
      means_plus <- c()
      means_minus <- c()
      for (i in 1:length(factors)) {
        plus <- filter_(data, paste(factors[i], "== 1", sep = ""))
        means_plus[i] <- mean(plus$Flugdauer)
        minus <- filter_(data, paste(factors[i], "== -1", sep = ""))
        means_minus[i] <- mean(minus$Flugdauer)
      }
      df <- data.frame(Faktoren = factors, Oben = means_plus, Unten = means_minus)
      plot <- ggplot(data = df) +
        facet_grid(facets = . ~ Faktoren) +
        geom_segment(mapping = aes(x = -1, xend = 1, y = Unten, yend = Oben)) +
        scale_x_continuous(name = "", breaks = c(-1, 1), minor_breaks = NULL, expand = c(0, 0)) +
        scale_y_continuous(name = "Durchschnittliche Flugdauer") +
        theme_bw()
      plot <- ggplotly(plot)
      return(plot)
    }
  })

  output$faktorstufen <- DT::renderDataTable({
    if (length_data_storage() != 0) {
      factors <- input$select_factors
      factors_max <- c("Fluegellaenge", "Koerperlaenge", "Einschnitt", "Papierstaerke")
      low <- c("60 mm", "50 mm", "0 mm", "80 g/mm^2")
      high <- c("90 mm", "100 mm", "60 mm", "120 g/mm^2")
      data <- data.frame(Faktor = factors_max, High = high, Low = low)
      values <- which(data$Faktor %in% factors)
      data[values,]
    }
  })

  ## Interaction Plot

  output$interaction_ui <- renderUI({
    if (input$tabset_main_versuchsplan == "Visualisierungen" && input$tabset_visualisierungen == "Interaction Plot") {
      tagList(
        fluidRow(
          column(
            width = 6,
            selectInput(
              inputId = ns("interaktion_faktor_1"),
              label = "Wähle den Faktor für die x-Achse",
              choices = c("Fluegellaenge", "Einschnitt", "Papierstaerke")
            )
          ),
          column(
            width = 6,
            selectInput(
              inputId = ns("interaktion_faktor_2"),
              label = "Wähle den Faktor für die Farbe",
              choices = c("Koerperlaenge", "Einschnitt", "Papierstaerke")
            )
          )
        )
      )
    }
  })

  observeEvent(input$interaktion_faktor_1, {
    selected <- input$interaktion_faktor_2
    updateSelectInput(session,
                      inputId = "interaktion_faktor_2",
                      choices = factor_names()[which(factor_names() != input$interaktion_faktor_1)],
                      selected = selected)
  })

  observeEvent(input$interaktion_faktor_2, {
    selected <- input$interaktion_faktor_1
    updateSelectInput(session,
                      inputId = "interaktion_faktor_1",
                      choices = factor_names()[which(factor_names() != input$interaktion_faktor_2)],
                      selected = selected)
  })

  output$interaction_plot <- renderPlotly({
    vp_means_interaction <- data_transform() %>%
      group_by_(input$interaktion_faktor_1, input$interaktion_faktor_2) %>%
      summarise(mean = mean(Flugdauer)) %>%
      ungroup
    print(vp_means_interaction)
    ll <- vp_means_interaction[which(vp_means_interaction[[input$interaktion_faktor_1]] == -1 & vp_means_interaction[[input$interaktion_faktor_2]] == -1),]
    lh <- vp_means_interaction[which(vp_means_interaction[[input$interaktion_faktor_1]] == -1 & vp_means_interaction[[input$interaktion_faktor_2]] == 1),]
    hl <- vp_means_interaction[which(vp_means_interaction[[input$interaktion_faktor_1]] == 1 & vp_means_interaction[[input$interaktion_faktor_2]] == -1),]
    hh <- vp_means_interaction[which(vp_means_interaction[[input$interaktion_faktor_1]] == 1 & vp_means_interaction[[input$interaktion_faktor_2]] == 1),]
    print(ll)
    plot_wechselwirkung <- ggplot() +
      geom_segment(mapping = aes(x = -1, xend = 1, y = ll$mean, yend = lh$mean, col = factor(-1)), size = 1.1) +
      geom_segment(mapping = aes(x = -1, xend = 1, y = hl$mean, yend = hh$mean, col = factor(1)), size = 1.1) +
      labs(x = input$interaktion_faktor_1, y = erklaert(), col = input$interaktion_faktor_2, title = "Interaction-Plot") +
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5, size = 22),
            axis.title = element_text(size = 18),
            axis.text = element_text(size = 16),
            legend.text = element_text(size = 16),
            legend.title = element_text(size = 18),
            panel.border = element_blank()) +
      scale_color_brewer(palette = "Blues") +
      scale_x_continuous(breaks = c(-1, 1), minor_breaks = NULL)
    plot <- ggplotly(plot_wechselwirkung)
    return(plot)
  })

  ## Pareto Plot

  output$pareto_ui <- renderUI({
    if (input$tabset_main_versuchsplan == "Visualisierungen" && input$tabset_visualisierungen == "Pareto Plot") {
      tagList(
        textInput(
          inputId = ns("pareto_signifikanzniveau"),
          label = "Signifikanzniveau",
          value = "0.05"
        )
      )
    }
  })

  output$pareto_plot <- renderPlotly({
    if (length_data_storage() != 0 && length_select_factors() != 0) {
      lm_1 <- lm_1()
      erklaert <- erklaert()
      alpha <- as.numeric(input$pareto_signifikanzniveau)
      effects <- summary(lm_1)$coefficients[,3][2:length(summary(lm_1)$coefficients[,3])]
      effect_names <- names(effects)
      factors_df <- factors()
      for (i in 1:length(factors_df$Faktor)) {
        effect_names <- str_replace_all(effect_names, factors_df$Faktor[i], factors_df$Label[i])
      }
      data <- data.frame(name = effect_names, effects = abs(effects))
      data$name <- factor(data$name, levels = data$name[order(data$effects, decreasing = TRUE)])
      plot <- ggplot(data = data) +
        geom_col(mapping = aes(x = name, y = effects)) +
        geom_hline(yintercept = abs(qt(alpha/2, df = df.residual(lm_1))), col = "red") +
        scale_x_discrete(name = NULL) +
        scale_y_continuous(name = "Flugdauer") +
        theme_bw() +
        ggtitle("Standardisierte Haupt- und Wechselwirkungen")
      plot <- ggplotly(plot)
      return(plot)
    }
  })

  ## Contour Plots

  lm_contour <- reactive({
    if (input$wechselwirkung == TRUE) {
      formel <- paste(erklaert(), " ~ ", input$contour_faktor_1, " * ", input$contour_faktor_2, sep = "")
    } else {
      formel <- paste(erklaert(), " ~ ", input$contour_faktor_1, " + ", input$contour_faktor_2, sep = "")
    }
    data_1 <- data_transform()
    print(formel)
    lm_2 <- lm(formel, data = data_1)
    return(lm_2)
  })

  ### 2D

  output$contour_plot_2D <- renderPlotly({
    if (length_data_storage() != 0) {
      data_1 <- data()
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
    if (input$tabset_main_versuchsplan == "Visualisierungen" && input$tabset_visualisierungen == "Contour Plots") {
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

  ## Residuenanalyse

  ### Residual Plot

  output$residual_plot <- renderPlotly({
    if (length_data_storage() != 0 && length_select_factors() != 0) {
      lm_1 <- lm_1()
      predicted <- lm_1$fitted.values
      residuals <- lm_1$residuals
      data <- data.frame(Fitted = predicted, Residuen <- residuals)
      plot <- ggplot(data = data) +
        geom_point(mapping = aes(x = Fitted, y = Residuen)) +
        theme_bw()
      plot <- ggplotly(plot)
      return(plot)
    }
  })

  output$normal_qq_plot <- renderPlotly({
    lm_1 <- lm_1()
    residuals <- lm_1$residuals
    ordered_residuals <- residuals[order(residuals)]
    theoretical_quantiles <- c()
    for (i in seq_along(ordered_residuals)) {
      theoretical_quantiles[i] <- qnorm(p = i/length(ordered_residuals) - 1 / (2 * length(ordered_residuals)))
    }
    data <- data.frame(theo = theoretical_quantiles, sample = ordered_residuals)
    lm_2 <- lm(sample ~ theo, data = data)
    plot <- ggplot(data = data) +
      geom_point(mapping = aes(x = theo, y = sample)) +
      labs(x = "Theoretische Quantile", y = "Stichprobenquantile") +
      geom_line(mapping = aes(x = theo, y = predict(lm_2)), col = "red") +
      theme_bw()
    plot <- ggplotly(plot)
    return(plot)
  })

  output$residual_centerpoints <- renderPlotly({
    lm_1 <- lm_1()
    data <- data_centerpoints()
    intercept <- unname(lm_1$coefficients[which(names(lm_1$coefficients) == "(Intercept)")])
    residuals <- data[[erklaert()]] - intercept
    plot <- ggplot() +
      geom_point(mapping = aes(x = seq_along(length(residuals)), y = residuals), col = "red") +
      geom_hline(yintercept = intercept, linetype = "dashed", col = "blue") +
      theme_bw()
    plot <- ggplotly(plot)
    return(plot)
  })
}
