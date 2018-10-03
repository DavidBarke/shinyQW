#' Run the QW-App
#'
#' @examples
#' run_app()
#'
#' @export
run_app <- function(app_directory = "app") {
  appDir <- system.file(app_directory, package = "shinyQW")
  if (appDir == "") {
    stop(paste0("Could not find ", app_directory, ". Try reinstalling `shinyQW`."), call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
