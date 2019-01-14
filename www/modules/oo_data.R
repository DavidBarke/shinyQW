versuchsplan <- tibble(
  x = runif(10, 0, 1),
  y = sample(c(-1, 1), 10, TRUE),
  z = sample(c(-1, 1), 10, TRUE)
)

#' @export
data_R6 <- R6::R6Class(
  "data",
  public = list(
    add_group = function(group) {
      req(group)
      private$datasets[[group]] <- reactiveValues()
    },
    add_dataset = function(group, name, value) {
      req(group, name, value)
      private$datasets[[group]][[name]] <- value
    },
    get_column = function(group, name, colname) {
      req(group, name, colname)
      self$get_dataset(group, name)[[colname]]
    },
    get_dataset = function(group, name) {
      req(group, name)
      private$datasets[[group]][[name]]
    },
    get_dataset_columns = function(group, name) {
      req(group, name)
      names(self$get_dataset(group, name))
    },
    get_datasets_names = function(group) {
      req(group)
      names(private$datasets[[group]])
    },
    get_group_names = function() {
      names(private$datasets)
    },
    switch_group = function(name, group_old, group_new, remove = TRUE) {
      dataset <- self$get_dataset(group_old, name)
      self$add_dataset(group_new, name, dataset)
      if (remove) {
        self$remove_dataset(group_old, name)
      }
    },
    remove_dataset = function(group, name) {
      rm(pos = .subset2(private$datasets[[group]], "impl")$.values, list = name)
    }
  ),
  private = list(
    datasets = list(
      "R-Datensätze" = reactiveValues(
        mtcars = mtcars,
        iris = iris,
        cars = cars
      ),
      "DQE-Beispieldatensätze" = reactiveValues(
        Versuchsplan = versuchsplan,
        SPC = pistonrings
      )
    )
  )
)
