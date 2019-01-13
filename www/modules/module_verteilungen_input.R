# Übersicht
# type: Art des Funktion (Dichte-, Verteilungs-, Quantilsfunktion)
# x_limits_method: Bestimmung der Achsenbegrenzungen (über Quantile oder über konkrete Werte)
# .mode: Art des Plots ("ideal", "rsr" (Zufallsstreubereiche))

module_verteilungen_input_header <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(
      width = 6,
      actionButton(
        inputId = ns("add_table"),
        label = label_lang(
          de = "Neue Tabelle",
          en = "Add table"
        )
      )
    ),
    column(
      width = 6,
      uiOutput(
        outputId = ns("select_input_table")
      )
    )
  )
}

module_verteilungen_input_tables <- function(id) {
  ns <- NS(id)
  
  div(
    id = ns("input_tables")
  )
}

module_verteilungen_input_remove_row_button <- function(id) {
  ns <- NS(id)
  
  actionButton(
    inputId = ns("remove_row"),
    label = label_lang(
      de = "Entferne Zeile",
      en = "Remove row"
    )
  )
}

module_verteilungen_input <- function(
  input, output, session, .data, .values, parent, 
  .mode, ...
) {
  self <- node$new("verteilungen_input", parent, session)
  
  ns <- session$ns
  
  .envir <- environment()
  
  # Möglicherweise können die Verteilungen auf Dauer als globale Variablen
  # definiert wird
  diskrete_verteilungen <- label_lang_list(
    de = c("Binomialverteilung", "Geometrische Verteilung",
           "Hypergeometrische Verteilung", "Multinomialverteilung",
           "Negative Binomialverteilung", "Poissonverteilung"),
    en = c("Binomial distribution", "Geometric Distribution",
           "Hypergeometric distribution", "Multinomial distribution",
           "Negative binomial distribution", "Poisson distribution"),
    value = c("binom", "geom", "hyper", "multinom", "nbinom", "pois")
  )
  
  stetige_verteilungen <- label_lang_list(
    de = c("Betaverteilung", "Cauchyverteilung",
           "Chi-Quadrat-Verteilung", "Exponentialverteilung",
           "F-Verteilung", "Gammaverteilung",
           "Gleichverteilung", "Log-Normalverteilung",
           "Normalverteilung", "t-Verteilung",
           "Weibullverteilung"),
    en = c("Beta distribution", "Cauchy distribution",
           "Chi-squared distribution", "Exponential distribution",
           "F-distribution", "Gamma distribution",
           "Uniform distribution", "Log-normal distribution",
           "Normal distribution", "Student's t-distribution",
           "Weibull distribution"),
    value = c("beta", "cauchy", "chisq", "exp", "f", "gamma", "unif", "lnorm",
              "norm", "t", "weibull")
  )
  
  rvs <- reactiveValues(
    # n_row ist ein Integer-Vektor, der für jede Tabelle die Anzahl der
    # verschiedenen Inputzeilen enthält
    n_row = integer(),
    n_table = 0,
    x_min = integer(),
    x_max = integer(),
    p_min = integer(),
    p_max = integer(),
    select_method_axes_limits = character(),
    .mode = .mode
  )
  
  output$select_input_table <- renderUI({
    n_table <- rvs$n_table
    if (n_table > 0) {
      ui <- selectInput(
        inputId = ns("select_input_table"),
        label = label_lang(
          de = "Wähle Tabelle",
          en = "Select table"
        ),
        choices = label_lang_list(
          de = paste("Tabelle", seq_len(n_table), sep = " "),
          en = paste("Table", seq_len(n_table), sep = " "),
          value = seq_len(n_table)
        ),
        selected = n_table
      )
      return(ui)
    }
  })
  
  observeEvent(input$add_table, {
    # Anpassen der rvs
    rvs$n_table <- rvs$n_table + 1
    n_table <- rvs$n_table
    rvs$n_row[n_table] <- 1
    rvs$x_min[n_table] <- 0
    rvs$x_max[n_table] <- 10
    rvs$p_min[n_table] <- 0.01
    rvs$p_max[n_table] <- 0.99
    rvs$select_method_axes_limits[n_table] <- "quantile"
    # trigger_x_limits_* triggert den reactive, der die x_limits berechnet. Der 
    # Umweg über die reactiveVal bzw. -Values (x/p_min/max) muss beschritten 
    # werden, da die Inputs in renderUI nur dann erreichbar sind, wenn diese 
    # auch dargestellt werden
    assign(
      envir = .envir,
      "trigger_x_limits" %_% n_table,
      reactiveVal(0)
    )
    # Einfügen in die Liste der möglichen Tabellen
    insertUI(
      selector = paste0("#", ns("input_tables")),
      where = "afterBegin",
      ui = uiOutput(
        outputId = ns("input_table" %_% n_table)
      )
    )
    output[["input_table" %_% n_table]] <- renderUI({
      if (input$select_input_table == n_table) { # Nur die ausgewählte Tabelle
        # wird angezeigt
        n_row <- rvs$n_row[n_table]
        if (rvs$.mode == "ideal") {
          distribution_choices <- label_lang_list(
            de = c("Diskrete Verteilungen", "Stetige Verteilungen"),
            en = c("Discrete distributions", "Contiuous distributions"),
            value = list(diskrete_verteilungen, stetige_verteilungen)
          )
        } else {
          distribution_choices <- stetige_verteilungen
        }
        ui <- tagList()
        for (i in seq_len(n_row)) {
          ui[[i]] <- div(
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = ns("select_distribution" %_% n_table %_% i),
                  label = label_lang(
                    de = paste0("Verteilung ", i),
                    en = paste0("Distribution ", i)
                  ),
                  choices = distribution_choices,
                  selected = fallback(
                    input[["select_distribution" %_% n_table %_% i]], 
                    "binom"
                  )
                )
              ),
              column(
                width = 8,
                get_specific_distribution_input(
                  session = session, 
                  input = input, 
                  .values = .values,
                  .mode = rvs$.mode,
                  index = n_table %_% i,
                  distribution = fallback(
                    input[["select_distribution" %_% n_table %_% i]], 
                    "binom"
                  )
                )
              )
            )
          )
        }
        ui <- div(
          ui,
          fluidRow(
            column(
              width = 4,
              actionButton(
                inputId = ns("add_plot" %_% n_table),
                label = label_lang(
                  de = "Neuer Plot",
                  en = "Add plot"
                )
              )
            ),
            column(
              width = 4,
              actionButton(
                inputId = ns("add_row" %_% n_table),
                label = label_lang(
                  de = "Neue Zeile",
                  en = "Add row"
                )
              )
            ),
            column(
              width = 4,
              actionButton(
                inputId = ns("remove_row" %_% n_table),
                label = label_lang(
                  de = "Entferne Zeile",
                  en = "Remove row"
                )
              )
            )
          )
        )
      }
    })
    observeEvent(input[["add_row" %_% n_table]], {
      rvs$n_row[n_table] <- rvs$n_row[n_table] + 1
    })
    observeEvent(input[["remove_row" %_% n_table]], {
      rvs$n_row[n_table] <- max(1, rvs$n_row[n_table] - 1)
    })
    observeEvent(input[["add_plot" %_% n_table]], {
      # Stelle sicher, dass die reactive() nur einmal erzeugt werden
      if (!.values$viewer$plot$is_value(
        ns("value_plot" %_% n_table)
        )) {
        assign(
          envir = .envir,
          "input_table" %_% n_table,
          reactive({
            # Die einzelnen Inputs der Tabellenzeilen werden als data.table von
            # get_arg_values() zurückgegeben und danach zu einer einzigen
            # data.table zusammengesetzt
            data_list <- list()
            for (i in seq_len(rvs$n_row[n_table])) {
              data_list[[i]] <- get_arg_values(
                session = session,
                distribution = fallback(input[["select_distribution" %_% n_table %_% i]], "binom"),
                index = i,
                ending = n_table %_% i
              )
            }
            data <- rbindlist(data_list)
          })
        )
        assign(
          envir = .envir,
          "input_short_table" %_% n_table,
          reactive({
            # input_short_table aggregiert aus den einzelnen Tabellenzeilen die
            # allgemeinen Werte wie Index, Verteilung und ob die Verteilung
            # diskret ist
            input_table <- get("input_table" %_% n_table)()
            input_short_table <- input_table[,.(index, distribution, discrete)][
              , head(.SD, 1), by = index
              ]
          })
        )
        assign(
          envir = .envir,
          "input_rsr_table" %_% n_table,
          reactive({
            if (rvs$.mode == "rsr") {
              data_list <- list()
              for (i in seq_len(rvs$n_row[n_table])) {
                data_list[[i]] <- input_rsr_table <- get_input_rsr_table(
                  session = session, 
                  index = i, 
                  ending = n_table %_% i
                )
              }
              data <- rbindlist(data_list)
            }
          })
        )
        assign(
          envir = .envir,
          "x_limits" %_% n_table,
          reactive({
            get("trigger_x_limits" %_% n_table)()
            input_table <- get("input_table" %_% n_table)()
            isolate({
              x_limits_method <- rvs$select_method_axes_limits[n_table]
              indices <- input_table$index
              x_limits <- get_x_limits(
                method = x_limits_method,
                indices = indices, 
                input_table = input_table,
                input_short_table = get("input_short_table" %_% n_table)(),
                rvs = rvs,
                n_table = n_table
              )
            })
            x_limits
          })
        )
        assign(
          envir = .envir,
          "distribution_plot" %_% n_table,
          reactive({
            type <- fallback(
              input[["select_plot_type" %_% n_table]], 
              "d"
            )
            input_table <- get("input_table" %_% n_table)()
            input_short_table <- get("input_short_table" %_% n_table)()
            input_rsr_table <- get("input_rsr_table" %_% n_table)()
            indices <- input_table$index
            if (type != "q") {
              x_limits <- get("x_limits" %_% n_table)()
            }
            # Für die einzelnen Tabellenzeilen werden basierend auf den Input-
            # werten Datensätze erzeugt, die später in add_trace() verwendet
            # werden
            p <- plot_ly(type = "scatter", mode = "lines")
            for (i in seq_len(max(indices))) {
              data <- get_distribution_data(
                i = i,
                input = input,
                type = type,
                n_table = n_table,
                input_table = input_table,
                input_short_table = input_short_table,
                input_rsr_table = input_rsr_table,
                x_limits = x_limits,
                .mode = rvs$.mode
              )
              p <- add_distribution_trace(
                index = i,
                p = p,
                type = type,
                .mode = rvs$.mode,
                data = data,
                name = input_short_table[i, distribution] %_% i,
                discrete = input_short_table[i, discrete]
              )
            }
            return(p)
          })
        )
        output[["distribution_plot" %_% n_table]] <- renderPlotly({
          p <- get("distribution_plot" %_% n_table)()
          return(p)
        })
        observeEvent(input[["modal_axes_limits" %_% n_table]], {
          showModal(
            ui = modalDialog(
              title = label_lang(
                de = "Achsenbegrenzungen",
                en = "Set axes limits"
              ),
              selectInput(
                inputId = ns("select_method_axes_limits" %_% n_table),
                label = label_lang(
                  de = "Setze Begrenzungen basierend auf",
                  en = "Set limits according to"
                ),
                choices = label_lang_list(
                  de = c("Konkreten Werten", "Quantilen stetiger Verteilungen"),
                  en = c("Concrete values", "Quantiles of continous distributions"),
                  value = c("concrete", "quantile")
                ),
                selected = rvs$select_method_axes_limits[n_table]
              ),
              uiOutput(
                outputId = ns("select_axes_limits_ui" %_% n_table)
              ),
              footer = div(
                modalButton(
                  label = label_lang(
                    de = "Verwerfen",
                    en = "Dismiss"
                  )
                ),
                actionButton(
                  inputId = ns("apply_axes_limits" %_% n_table),
                  label = label_lang(
                    de = "Anwenden",
                    en = "Apply"
                  )
                ),
                actionButton(
                  inputId = ns("apply_all_axes_limits" %_% n_table),
                  label = label_lang(
                    de = "Auf alle Verteilungsplots anwenden",
                    en = "Apply to all distribution plots"
                  )
                )
              )
            )
          )
        })
        observeEvent(input[["apply_axes_limits" %_% n_table]], {
          get("trigger_x_limits" %_% n_table)(get("trigger_x_limits" %_% n_table)() + 1)
          x_limits_method <- input[["select_method_axes_limits" %_% n_table]]
          rvs$select_method_axes_limits[n_table] <- x_limits_method
          if (x_limits_method == "concrete") {
            rvs$x_min[n_table] <- input[["x_min" %_% n_table]]
            rvs$x_max[n_table] <- input[["x_max" %_% n_table]]
          } else {
            rvs$p_min[n_table] <- input[["p_limits" %_% n_table]][1]
            rvs$p_max[n_table] <- input[["p_limits" %_% n_table]][2]
          }
          removeModal()
        })
        observeEvent(input[["apply_all_axes_limits" %_% n_table]], {
          x_limits_method <- input[["select_method_axes_limits" %_% n_table]]
          rvs$select_method_axes_limits <- rep(x_limits_method, times = rvs$n_table)
          if (x_limits_method == "concrete") {
            rvs$x_min <- rep(input[["x_min" %_% n_table]], times = rvs$n_table)
            rvs$x_max <- rep(input[["x_max" %_% n_table]], times = rvs$n_table)
          } else {
            rvs$p_min <- rep(input[["p_limits" %_% n_table]][1], times = rvs$n_table)
            rvs$p_max <- rep(input[["p_limits" %_% n_table]][2], times = rvs$n_table)
          }
          for (i in seq_len(rvs$n_table)) {
            get("trigger_x_limits" %_% i)(get("trigger_x_limits" %_% i)() + 1)
          }
          removeModal()
        })
        output[["select_axes_limits_ui" %_% n_table]] <- renderUI({
          if (input[["select_method_axes_limits" %_% n_table]] == "concrete") {
            fluidRow(
              column(
                width = 6,
                numericInput(
                  inputId = ns("x_min" %_% n_table),
                  label = label_lang(
                    de = "Minimaler Wert",
                    en = "Minimal value"
                  ),
                  value = fallback(
                    rvs$x_min[n_table],
                    0
                  )
                )
              ),
              column(
                width = 6,
                numericInput(
                  inputId = ns("x_max" %_% n_table),
                  label = label_lang(
                    de = "Maximaler Wert",
                    en = "Maximal value"
                  ),
                  value = fallback(
                    rvs$x_max[n_table],
                    10
                  )
                )
              )
            )
          } else {
            sliderInput(
              inputId = ns("p_limits" %_% n_table),
              label = label_lang(
                de = "Quantile für stetige Verteilungen",
                en = "Quantile of continous distributions"
              ),
              value = fallback(
                c(rvs$p_min[n_table], rvs$p_max[n_table]),
                c(0.01, 0.99)
              ),
              min = 0.01,
              max = 0.99,
              step = 0.01
            )
          }
        })
      }
      if (rvs$.mode == "ideal") {
        select_plot_type_col <- column(
          width = 4,
          selectInput(
            inputId = ns("select_plot_type" %_% n_table),
            label = label_lang(
              de = "Plottyp",
              en = "Plot type"
            ),
            choices = label_lang_list(
              de = c(
                "Wahrscheinlichkeitsdichtefunktion", 
                "Verteilungsfunktion",
                "Quantilsfunktion"
              ),
              en = c(
                "Density function",
                "Cumulative distribution function",
                "Quantile function"
              ),
              value = c("d", "p", "q")
            )
          )
        )
      } else {
        select_plot_type_col <- NULL
      }
      .values$viewer$plot$append_tab(
        tab = tabPanel(
          title = label_lang(
            de = paste0("Verteilung: ", n_table),
            en = paste0("Distribution: ", n_table)
          ),
          fluidRow(
            select_plot_type_col,
            column(
              width = 4,
              numericInput(
                inputId = ns("continous_steps" %_% n_table),
                label = label_lang(
                  de = "Stetige Stützstellen",
                  en = "Continous support points"
                ),
                value = 50,
                min = 2
              )
            ),
            column(
              width = 4,
              actionButton(
                inputId = ns("modal_axes_limits" %_% n_table),
                label = label_lang(
                  de = "Achsenbegrenzungen",
                  en = "Set axes limits"
                )
              )
            )
          ),
          plotlyOutput(
            outputId = ns(
              "distribution_plot" %_% n_table
            )
          ),
          value = ns("value_plot" %_% n_table)
        )
      )
    })
  })
  
}