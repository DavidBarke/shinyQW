#' Create a dashboard box without padding
#'
#' @export
full_box <- function(..., title = NULL, footer = NULL, status = NULL,
                     solidHeader = FALSE, background = NULL, width = 6,
                     height = NULL, collapsible = FALSE, collapsed = FALSE) {
  h <- box(..., title = title, footer = footer, status = status,
           solidHeader = solidHeader, background = background, width = width,
           height = height, collapsible = collapsible, collapsed = collapsed)
  h$attribs$style <- "padding: 0px"
  h
}

#' Create a dashboard body without padding
#'
#' @export
full_dashboardBody <- function(...) {
  h <- dashboardBody(...)
  # h$children[[1]]$attribs$style <- "padding: 0px"
  # h
}
