# Veraltet
R6_verteilungsmodelle <- R6Class(
  classname = "verteilungsmodelle",
  public = list(
    initialize = function(session, .values, index = NULL) {
      private$session <- session
      private$.values <- .values
      private$count_extern <- index
      private$diskrete_verteilungen <- label_lang_list(
        de = c("Binomialverteilung", "Geometrische Verteilung",
               "Hypergeometrische Verteilung", "Multinomialverteilung",
               "Negative Binomialverteilung", "Poissonverteilung"),
        en = c("Binomial distribution", "Geometric Distribution",
               "Hypergeometric distribution", "Multinomial distribution",
               "Negative binomial distribution", "Poisson distribution"),
        value = c("binom", "geom", "hyper", "multinom", "nbinom", "pois")
      )
      private$stetige_verteilungen <- label_lang_list(
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
    },
    
    add_input_row = function() {
      current_selected_distributions <- character()
      for (i in seq_len(private$count_intern)) {
        current_selected_distributions[i] <- private$session$input[["select_distribution" %_% i]]
      }
      print(current_selected_distributions)
      private$count_intern <- private$count_intern + 1
      private$selected_distributions <- c(current_selected_distributions, "binom")
    },
    
    get_input_ui = function() {
      ui <- tagList()
      for (i in seq_len(private$count_intern)) {
        ui[[i]] <- fluidRow(
          column(
            width = 4,
            selectInput(
              inputId = private$session$ns("select_distribution" %_% i),
              label = label_lang(
                de = "Verteilung",
                en = "Distribution"
              ),
              choices = label_lang_list(
                de = c("Diskrete Verteilungen", "Stetige Verteilungen"),
                en = c("Discrete distributions", "Contiuous distributions"),
                value = list(private$diskrete_verteilungen, 
                             private$stetige_verteilungen)
              )
            )
          ),
          column(
            width = 8,
            get_specific_distribution_input(
              session = private$session, 
              input = private$session$input, 
              .values = private$.values, 
              index = i, 
              distribution = private$selected_distributions[i]
            )
          )
        )
      }
      for (i in seq_len(private$count_intern)) {
        updateSelectInput(
          session = private$session,
          inputId = "select_distribution" %_% i,
          selected = private$selected_distributions[i]
        )
      }
      return(ui)
    }
  ),
  private = list(
    session = NULL,
    .values = NULL,
    count_extern = NULL,
    count_intern = 1,
    selected_distributions = "binom",
    diskrete_verteilungen = NULL,
    stetige_verteilungen = NULL
  )
)
