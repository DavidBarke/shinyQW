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
  h$children[[1]]$attribs$style <- "padding: 0px"
  h
}

#' Create a collapsible tabbed box
#'
#' Create a collapsible \code{\link[shinydashboard]{tabBox}}.
#'
#' @param ... \code{\link[shiny]{tabPanel}} elements to include in the tabset
#' @param id If provided, you can use \code{input$id} in your server logic to
#' determine which of the current tabs is active. The value will correspond to
#' the \code{value} argument that is passed to \code{\link[shiny]{tabPanel}}.
#' @param selected The \code{value} (or, if none was supplied, the \code{title})
#' of the tab that should be selected by default. If \code{NULL}, the first tab
#' will be selected.
#' @param title Title for the tabBox.
#' @param width The width of the box, using the Bootstrap grid system. This is
#' used for row-based layouts. The overall width of a region is 12, so the
#' default valueBox width of 4 occupies 1/3 of that width. For column-based
#' layouts, use \code{NULL} for the width; the width is set by the column that
#' contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#' the height scales automatically with the content.
#' @param side Which side of the box the tabs should be on (\code{"left"} or
#' \code{"right"}). When \code{side="right"}, the order of tabs will be
#' reversed.
#'
#' @details
#' The created \code{\link[shinydashboard]{tabBox}}'s class attribute contains
#' "collapsible-tab-box".
#'
#' @export
collapsible_tabBox <- function(
  ..., id = NULL, selected = NULL, title = NULL, width = 6, height = NULL,
  side = c("left", "right")
) {
  ui <- shinydashboard::box(
    title = title,
    collapsible = TRUE,
    width = width,
    height = height,
    shinydashboard::tabBox(
      id = id,
      width = 12,
      side = match.arg(side),
      selected = selected,
      ...
    )
  )
  ui$children[[1]]$children[[2]]$children[[1]]$attribs$class <- paste(
    ui$children[[1]]$children[[2]]$children[[1]]$attribs$class,
    "collapsible-tab-box"
  )
  ui
}
