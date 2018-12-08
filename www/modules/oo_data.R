#' @export
data_R6 <- R6::R6Class(
  "data",
  public = list(
    add_group = function(group) {
      private$datasets[[group]] <- reactiveValues()
    },
    add_dataset = function(group, name, value) {
      private$datasets[[group]][[name]] <- value
    },
    get_dataset = function(group, name) {
      private$datasets[[group]][[name]]
    },
    get_datasets_names = function(group) {
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
      user_data_storage = reactiveValues(),
      permanent_data_storage = reactiveValues(
        mtcars = mtcars,
        iris = iris,
        cars = cars
      )
    )
  )
)
