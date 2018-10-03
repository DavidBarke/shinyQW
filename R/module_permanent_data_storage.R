#' @export
initial_permanent_data_storage <- function() {
  initial_permanent_data_storage <- list(
    mtcars = mtcars,
    iris = iris,
    diamonds = diamonds,
    volcano = volcano
  )
  return(initial_permanent_data_storage)
}
