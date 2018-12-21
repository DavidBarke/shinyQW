# TODO: unique(indices) durch seq_len(max(indices)) ersetzen

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
      selectInput(
        inputId = ns("select_input_table"),
        label = label_lang(
          de = "Wähle Tabelle",
          en = "Select table"
        ),
        choices = NULL
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
    n_table = 0
  )
  
  observeEvent(input$add_table, {
    # Anpassen der rvs
    rvs$n_table <- rvs$n_table + 1
    n_table <- rvs$n_table
    rvs$n_row[n_table] <- 1
    # Anpassen der Tabellenauswahl
    updateSelectInput(
      session = session,
      inputId = "select_input_table",
      choices = label_lang_list(
        de = paste("Tabelle", seq_len(n_table), sep = " "),
        en = paste("Table", seq_len(n_table), sep = " "),
        value = seq_len(n_table)
      ),
      selected = n_table
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
        ui <- tagList()
        for (i in seq_len(n_row)) {
          ui[[i]] <- div(
            fluidRow(
              column(
                width = 4,
                selectInput(
                  inputId = ns("select_distribution" %_% n_table %_% i),
                  label = label_lang(
                    de = "Verteilung",
                    en = "Distribution"
                  ),
                  choices = label_lang_list(
                    de = c("Diskrete Verteilungen", "Stetige Verteilungen"),
                    en = c("Discrete distributions", "Contiuous distributions"),
                    value = list(diskrete_verteilungen, stetige_verteilungen)
                  ),
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
        if (.mode == "rsr") {
          ui <- div(
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  inputId = ns("n_sided" %_% n_table),
                  label = label_lang(
                    de = "Zufallsstreubereich",
                    en = "Random scattering range"
                  ),
                  choices = label_lang_list(
                    de = c("Einseitig", "Zweiseitig"),
                    en = c("One-sided", "Two-sided"),
                    value = c("one", "two")
                  )
                )
              ),
              column(
                width = 6,
                sliderInput(
                  inputId = ns("alpha" %_% n_table),
                  label = "Alpha",
                  min = 0,
                  max = 1,
                  value = 0.05,
                  step = 0.01
                )
              )
            ),
            ui
          )
        }
        ui
      }
    })
    observeEvent(input[["add_row" %_% n_table]], {
      rvs$n_row[n_table] <- rvs$n_row[n_table] + 1
    })
    observeEvent(input[["remove_row" %_% n_table]], {
      rvs$n_row[n_table] <- max(1, rvs$n_row[ntable] - 1)
    })
    observeEvent(input[["add_plot" %_% n_table]], {
      # Stelle sicher, dass die reactive() nur einmal erzeugt werden
      if (!.values$viewer$plot$is_value(
        ns("value_plot" %_% n_table)
        )) {
        assign(
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
          "input_short_table" %_% n_table,
          reactive({
            # input_short_table extrahiert aus den einzelnen Tabellenzeilen die
            # allgemeinen Werte wie Index, Verteilung und ob die Verteilung
            # diskret ist
            input_table <- get("input_table" %_% n_table)()
            input_short_table <- input_table[,.(index, distribution, discrete)][
              , head(.SD, 1), by = index
              ]
          })
        )
        assign(
          "distribution_plot" %_% n_table,
          reactive({
            type <- fallback(
              input[["select_plot_type" %_% n_table]], 
              "d"
            )
            input_table <- get("input_table" %_% n_table)()
            input_short_table <- get("input_short_table" %_% n_table)()
            indices <- input_table$index
            if (type != "q") {
              method <- fallback(
                input[["select_method_axes_limits" %_% n_table]],
                "quantile"
              )
              if (method == "quantile") {
                x_limits <- get_x_limits(
                  indices = indices, 
                  input_table = input_table,
                  input_short_table = input_short_table,
                  p_limits = input[["p_limits" %_% n_table]]
                )
              } else {
                x_limits <- c(
                  input[["x_min" %_% n_table]], 
                  input[["x_max" %_% n_table]]
                )
              }
            }
            # Für die einzelnen Tabellenzeilen werden basierend auf den Input-
            # werten Datensätze erzeugt, die später in add_trace() verwendet
            # werden
            for (i in seq_len(max(indices))) {
              assign(
                "data" %_% i,
                get_distribution_data(
                  i = i,
                  input = input,
                  type = type,
                  n_table = n_table,
                  input_table = input_table,
                  input_short_table = input_short_table,
                  x_limits = x_limits
                )
              )
            }
            p <- plot_ly(type = "scatter", mode = "lines")
            for (i in seq_len(max(indices))) {
              p <- get_distribution_trace(
                p = p,
                type = type,
                x = get("data" %_% i)$x,
                y = get("data" %_% i)$y,
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
      }
      .values$viewer$plot$append_tab(
        tab = tabPanel(
          title = label_lang(
            de = paste0("Verteilung: ", n_table),
            en = paste0("Distribution: ", n_table)
          ),
          fluidRow(
            column(
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
            ),
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
                inputId = ns("select_axes_limits" %_% n_table),
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
      observeEvent(input[["select_axes_limits" %_% n_table]], {
        showModal(
          ui = modalDialog(
            title = label_lang(
              de = "Achsenbegrenzungen",
              en = "Set axes limits"
            ),
            footer = modalButton(
              label = label_lang(
                de = "Schließen",
                en = "Dismiss"
              )
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
              selected = "quantile"
            ),
            uiOutput(
              outputId = ns("select_axes_limits_ui" %_% n_table)
            )
          )
        )
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
                value = 0
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
                value = 10
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
            value = c(0.01, 0.99),
            min = 0.01,
            max = 0.99,
            step = 0.01
          )
        }
      })
    })
  })
  
}