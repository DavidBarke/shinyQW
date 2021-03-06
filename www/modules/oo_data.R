versuchsplan <- tibble(
  x = runif(10, 0, 1),
  y = sample(c(-1, 1), 10, TRUE),
  z = sample(c(-1, 1), 10, TRUE)
)

data_check <- function(data, check) {
  NULL
}

data_container_R6 <- R6Class(
  classname = "data_container",
  public = list(
    initialize = function(data, ...) {
      private$dataset <- reactiveVal(data)
    },
    
    get_dataset = function() {
      private$dataset()
    },
    
    get_object_information = function() {
      private$object_information
    },
    
    update_dataset = function(new_dataset) {
      private$dataset(new_dataset)
    }
  ),
  private = list(
    dataset = NULL,
    object_information = list(
      
    )
  )
)

#' @export
data_R6 <- R6::R6Class(
  "data",
  lock_objects = FALSE,
  public = list(
    initialize = function() {
      self$get_group_names <- reactive({
        self$trigger_group_names()
        names(private$data_containers)
      })
      self$trigger_group_names <- reactiveVal(0)
      
      self$datasets_names <- reactive({
        self$trigger_datasets_names()
        names(private$data_containers[[self$selected_group()]])
      })
      self$trigger_datasets_names <- reactiveVal(0)
      self$selected_group <- reactiveVal(character())
    },
    
    add_group = function(group) {
      req(group)
      private$data_containers[[group]] <- list()
      self$trigger_group_names(self$trigger_group_names() + 1)
    },
    
    add_dataset = function(group, name, dataset) {
      req(group, name, dataset)
      if (is.null(private$data_containers[[group]][[name]])) {
        data_container <- data_container_R6$new(dataset, check = NULL)
        private$data_containers[[group]][[name]] <- data_container
        self$trigger_datasets_names(self$trigger_datasets_names() + 1)
      } else {
        private$data_containers[[group]][[name]]$update_dataset(dataset)
      }
    },
    
    get_column = function(group, name, colname) {
      self$get_dataset(group, name)[[colname]]
    },
    
    get_dataset = function(group, name) {
      data <- req(private$data_containers[[group]][[name]])$get_dataset()
    },
    
    get_dataset_columns = function(group, name) {
      req(group, name)
      names(self$get_dataset(group, name))
    },
    
    get_datasets_names = function(group) {
      req(group)
      self$selected_group(group)
      self$datasets_names()
    },
    
    get_object_information = function(group, name) {
      private$data_containers[[group]][[name]]$get_object_information()
    },
    
    switch_group = function(name, group_old, group_new, remove = TRUE) {
      dataset <- self$get_dataset(group_old, name)
      self$add_dataset(group_new, name, dataset)
      if (remove) {
        self$remove_dataset(group_old, name)
      }
    },
    
    remove_dataset = function(group, name) {
      private$data_containers[[group]][[name]] <- NULL
      self$trigger_datasets_names(self$trigger_datasets_names() + 1)
    }
  ),
  private = list(
    data_containers = list(
      "R-Datensaetze" = list(
        mtcars = data_container_R6$new(mtcars),
        iris = data_container_R6$new(iris),
        diamonds = data_container_R6$new(diamonds),
        cars = data_container_R6$new(cars)
      ),
      "DQE-Datensaetze" = list(
        Versuchsplan = data_container_R6$new(versuchsplan),
        SPC = data_container_R6$new(pistonrings)
      )
    )
  )
)
