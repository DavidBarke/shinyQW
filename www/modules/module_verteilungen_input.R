# TODO: unique(indices) durch seq_len(max(indices)) ersetzen

module_verteilungen_input_header <- function(id) {
  ns <- NS(id)
  
  fluidRow(
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
    ),
    column(
      width = 6,
      actionButton(
        inputId = ns("add_table"),
        label = label_lang(
          de = "Neue Tabelle",
          en = "Add table"
        )
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
  input, output, session, .data, .values, parent, ...
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
            )
          )
        )
      }
    })
    observeEvent(input[["add_row" %_% n_table]], {
      rvs$n_row[n_table] <- rvs$n_row[n_table] + 1
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
            p_min = 0.01
            p_max = 0.99
            type <- fallback(
              input[["select_plot_type" %_% n_table]], 
              "d"
            )
            input_table <- get("input_table" %_% n_table)()
            input_short_table <- get("input_short_table" %_% n_table)()
            indices <- input_table$index
            if (type != "q") {
              # Für Dichte- und Verteilungsfunktion müssen die Grenzen des Plots
              # basierend auf allen Tabellenzeilen erstellt werden. Dabei muss
              # zwischen diskreten und stetigen Verteilungen unterschieden
              # werden und innerhalb der diskreten Verteilungen zwischen Vertei-
              # lungen deren Definitionsbereich beschränkt bzw. unbeschränkt
              # sind
              xmax_rows <- input_table[name == "xmax"]
              xmax_indices <- xmax_rows$index
              x_min <- numeric(length(unique(indices)))
              x_max <- numeric(length(unique(indices)))
              for (i in seq_len(max(indices))) {
                subset_table <- input_table[index == i]
                if (i %in% xmax_indices) {
                  arg_values <- subset_table[name != "xmax", value]
                  names(arg_values) <- subset_table[name != "xmax", name]
                  subset_args <- as.list(arg_values)
                  x_min[i] <- 0
                  x_max[i] <- subset_table[name == "xmax", value]
                } else {
                  arg_values <- subset_table[, value]
                  names(arg_values) <- subset_table[, name]
                  subset_args <- as.list(arg_values)
                  if (any(subset_table[, discrete])) {
                    x_min[i] <- 0
                    x_max[i] <- do.call(
                      what = paste0("q", input_short_table[i, distribution]),
                      args = c(list(p = 1), subset_args)
                    )
                  } else {
                    x_min[i] <- do.call(
                      what = paste0("q", input_short_table[i, distribution]),
                      args = c(list(p = p_min), subset_args)
                    )
                    x_max[i] <- do.call(
                      what = paste0("q", input_short_table[i, distribution]),
                      args = c(list(p = p_max), subset_args)
                    )
                  }
                }
              }
              x_min_min <- floor(min(x_min))
              x_max_max <- ceiling(max(x_max))
              x_int <- x_min_min:x_max_max
              x_seq <- seq(
                x_min_min, x_max_max, 
                length.out = fallback(
                  input[["continous_steps" %_% n_table]], 
                  50
                )
              )
            } else {
              # Für die Quantilsfunktion sind die Grenzen der x-Achse trivial
              p <- seq(0, 1, length.out = 100)
            }
            # Für die einzelnen Tabellenzeilen werden basierend auf den Input-
            # werten Datensätze erzeugt, die später in add_trace() verwendet
            # werden
            for (i in seq_len(max(indices))) {
              subset_table <- input_table[index == i]
              arg_values <- subset_table[name != "xmax", value]
              names(arg_values) <- subset_table[name != "xmax", name]
              subset_args <- as.list(arg_values)
              if (type != "q") {
                discrete <- input_short_table[i, discrete]
                if (discrete) {
                  x <- x_int
                } else {
                  x <- x_seq
                }
                x_var <- "x"
                if (type == "d") {
                  first_arg_list <- list(x = x)
                } else {
                  first_arg_list <- list(q = x)
                }
              } else {
                first_arg_list <- list(p = p)
                x_var <- "p"
              }
              assign(
                "data" %_% i,
                data.table(
                  x = get(x_var),
                  y = do.call(
                    what = paste0(type, input_short_table[i, distribution]),
                    args = c(first_arg_list, subset_args)
                  )
                )
              )
            }
            p <- plot_ly(type = "scatter", mode = "lines")
            for (i in seq_len(max(indices))) {
              p <- get_distribution_trace(
                p = p,
                x = get("data" %_% i)$x,
                y = get("data" %_% i)$y,
                name = input_short_table[i, distribution] %_% i,
                discrete =input_short_table[i, discrete]

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