#' Run the QW-App
#'
#' @examples
#' run_app()
#'
#' @export
run_app <- function() {
  appDir <- system.file("app", package = "shinyQW")
  if (appDir == "") {
    stop("Could not find myapp. Try reinstalling `shinyQW`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
