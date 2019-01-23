#' @export
dqe_design_of_experiments_versuchsplan_box <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_default_ui(
      id = ns("id_data_selector")
    ),
    fluidRow(
      column(
        width = 8,
        uiOutput(
          outputId = ns("select_factors")
        )
      ),
      column(
        width = 4,
        uiOutput(
          outputId = ns("column_warnings")
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        selectInput(
          inputId = ns("select_plot_type"),
          label = label_lang(
            de = "Wähle Plot",
            en = "Select plot"
          ),
          choices = label_lang_list(
            de = c("Paretoplot", "Effektplot", "Interaktionsplot", "Contourplot"),
            en = c("Pareto plot", "Effect plot", "Interaction plot", "Contour plot"),
            value = c("pareto", "effect", "interaction", "contour")
          )
        )
      ),
      column(
        width = 6,
        actionButton(
          inputId = ns("add_plot"),
          label = label_lang(
            de = "Neuer Plot",
            en = "Add plot"
          )
        )
      )
    )
  )
}

#' @export
dqe_design_of_experiments_versuchsplan <- function(
  input, output, session, .data, .values, parent, ...
) {
  self <- node$new("versuchsplan", parent, session)

  ns <- session$ns
  
  rvs <- reactiveValues(
    wrong_column_format = TRUE
  )
  
  warnings <- reactiveValues(
    wrong_format_factor = FALSE,
    wrong_format_factors = character(),
    wrong_format_target = FALSE
  )
  
  output$select_factors <- renderUI({
    names_selected_data <- names(selected_data())
    choices_factors <- setdiff(names_selected_data, input$selected_target)
    tagList(
      selectizeInput(
        inputId = ns("selected_factors"),
        label = label_lang(
          de = "Faktoren",
          en = "Factor variables"
        ),
        choices = choices_factors,
        selected = fallback(input$selected_factors, choices_factors[1]),
        multiple = TRUE
      ),
      selectInput(
        inputId = ns("selected_target"),
        label = label_lang(
          de = "Zielgröße",
          en = "Target variable"
        ),
        choices = names_selected_data,
        selected = fallback(input$selected_target, names_selected_data[1])
      )
    )
  })
  
  output$column_warnings <- renderUI({
    warning <- character()
    if (warnings$wrong_format_factor) {
      warning <- paste(warning, label_lang(
        de = "Faktoren dürfen höchstens drei Ausprägungen (Hoch, Tief, '0') haben.",
        en = "Warning"
      ))
    }
    if (warnings$wrong_format_target) {
      warning <- paste(warning, label_lang(
        de = "Zielgröße muss numerische Werte annehmen",
        en = "Target variable must be numeric"
      ))
    }
    div(
      class = "warning",
      warning
    )
  })

  # Reactives
  
  selected_factor_data <- reactive({
    data <- as.tibble(selected_data())
    data[, names(data) %in% input$selected_factors]
  })
  
  selected_target_data <- reactive({
    data <- as.tibble(selected_data())
    if (!is.numeric(data[[input$selected_target]])) {
      warnings$wrong_format_target <- TRUE
    } else {
      warnings$wrong_format_target <- FALSE
    }
    data[, input$selected_target]
  })
  
  selected_factor_data_mapped <- reactive({
    data <- selected_factor_data()
    wrong_format_factor <- FALSE
    for (i in seq_along(data)) {
      if (length(unique(data[[i]])) > 3) {
        wrong_format_factor <- TRUE
        wrong_format_factors <- c(warnings$wrong_format_factors, names(data)[i])
        break
      } else {
        max <- max(data[,i])
        min <- min(data[,i])
        data[,i] <- maprange(data[,i], min, max, -1, 1)
      }
    }
    warnings$wrong_format_factor <- wrong_format_factor
    if (!wrong_format_factor) {
      wrong_format_factors <- character()
    }
    data
  })
  
  combined_data <- reactive({
    data <- bind_cols(
      selected_target_data(),
      selected_factor_data_mapped()
    )
  })
  
  count_selected_factors <- reactive({
    length(input$selected_factors)
  })
  
  lm_data <- reactive({
    combined_data()
  })
  
  lm_formula <- reactive({
    anzahl <- max(2, count_selected_factors)
    as.formula(paste0(input$selected_target, " ~ . ^ ", anzahl))
  })
  
  lm_coefficients <- reactive({
    linear_model()$coefficients
  })
  
  linear_model <- reactive({
    linear_model <- lm(lm_formula(), lm_data())
    linear_model$call <- lm_formula()
    linear_model
  })
  
  # 
  # factors <- reactive({
  #   factors <- c("Fluegellaenge", "Koerperlaenge", "Einschnitt", "Papierstaerke")
  #   letters <- as.character(LETTERS[1:length(factors)])
  #   data <- data.frame(Faktor = factors, Label = letters)
  #   data$Label <- as.character(data$Label)
  #   return(data)
  # })
  # 
  # selected_factors <- reactive({
  #   data <- factors()
  #   which <- which(data$Faktor %in% input$select_factors)
  #   data <- data[which,]
  #   data$Label <- as.character(data$Label)
  #   return(data)
  # })

  ## Lineares Modell

  # output$summary_linear_model <- renderPrint({
  #   if (!is.null(input$select_factors)) {
  #     lm_1 <- lm_1()
  #     s <- summary(lm_1)
  #     factors_df <- factors()
  #     effect_names <- row.names(s$coefficients)
  #     for (i in 1:length(factors_df$Faktor)) {
  #       effect_names <- str_replace_all(effect_names, factors_df$Faktor[i], factors_df$Label[i])
  #     }
  #     row.names(s$coefficients) <- effect_names
  #     print(s)
  #   }
  # })
  # 
  # output$vereinfachtes_modell <- renderUI({
  #   checkboxInput(
  #     inputId = ns("vereinfachtes_modell"),
  #     label = "Vereinfachtes Modell"
  #   )
  # })

  # Visualisierungen
  
  ## Handling
  
  observeEvent(input$add_plot, {
    output[[input$select_plot_type %_% "plot"]] <- switch(
      input$select_plot_type,
      "pareto" = renderPlotly({
        
      }),
      "effect" = renderPlotly({
        effect_plot()
      }),
      "interaction" = renderPlotly({
        
      }),
      "contour" = renderPlotly({
        
      })
    )
    .values$viewer$plot$append_tab(
      tab = tabPanel(
        title = label_lang(
          de = "Plot",
          en = "Plot"
        ),
        value = ns(input$select_plot_type %_% "plot"),
        plotlyOutput(
          outputId = ns(input$select_plot_type %_% "plot")
        )
      )
    )
  })
  
  ## Effect Plot

  effect_plot <- reactive({
    effect_plot <- doe_effect_plot(
      data = combined_data()
    )
  })

  output$faktorstufen <- DT::renderDataTable({
    factors <- input$select_factors
    factors_max <- c("Fluegellaenge", "Koerperlaenge", "Einschnitt", "Papierstaerke")
    low <- c("60 mm", "50 mm", "0 mm", "80 g/mm^2")
    high <- c("90 mm", "100 mm", "60 mm", "120 g/mm^2")
    data <- data.frame(Faktor = factors_max, High = high, Low = low)
    values <- which(data$Faktor %in% factors)
    data[values,]
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

  interaction_plot <- reactive({
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

  pareto_plot <- reactive({
    if (length_select_factors() != 0) {
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

  contour_plot_2D <- reactive({
    data_1 <- selected_data()
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
  })

  ### 3D

  contour_plot_3D <- reactive({
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

  residual_plot <- reactive({
    if (length_select_factors() != 0) {
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

  normal_qq_plot <- reactive({
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

  residual_centerpoints <- reactive({
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
  
  selected_data <- callModule(
    module = data_selector,
    id = "id_data_selector",
    .data = .data,
    .values = .values,
    parent = self
  )
}
