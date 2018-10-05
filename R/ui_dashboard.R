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
#' @param closeable If the \code{\link{collapsible_tabBox}} is closeable via an
#' \code{\link[shiny]{actionButton}} use \code{closeable=TRUE}.
#'
#' @details
#' The \code{collapsible_tabBox} has class "collapsible-tab-box", the enclosing
#' div of the closing \code{\link[shiny]{actionButton}} has class
#' "div-btn-close".
#'
#' @export
collapsible_tabBox <- function(
  ..., id = NULL, selected = NULL, title = NULL, width = 6, height = NULL,
  side = c("left", "right"), closeable = FALSE
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
  if (closeable) {
    ui$children[[1]]$children[[1]]$children[[2]]$children[[2]] <- div(
      class = "div-btn-close",
      shiny::actionButton(
        inputId = id %_% "close",
        label = NULL,
        icon = icon("window-close")
      )
    )
  }
  ui
}

# TODO: Use ... argument of actionButton
#' Action item
#'
#' Create a \code{\link[shinydashboard:sidebarMenu]{menuItem}}-like
#' \code{\link[shiny]{actionButton}} with an enclosing div with class
#' "div-btn-sidebar".
#'
#' @param inputId The \code{input} slot that will be used to access the value.
#' @param label The contents of the button or link - usually a text label, but
#' you could also use any other HTML, like an image.
#' @param An optional \code{\link[shiny]{icon}} to appear on the button.
#' @param width The width of the input, e.g. \code{'400px'}, or \code{'100\%'};
#' see \code{\link[shiny]{validateCssUnit}}.
#'
#' @export
actionItem <- function(inputId, label, ...) {
  if (missing(...)) {
    ui <- tags$li(
      `data-toggle` = "tab",
      `data-value` = "tab",
      tags$a(
        div(
          class = "div-btn-sidebar",
          shiny::actionButton(
            inputId = inputId,
            label = label
          )
        )
      )
    )
  } else {
    ui <- tags$li(
      class = "treeview",
      tags$a(
        div(
          class = "div-btn-sidebar",
          shiny::actionButton(
            inputId = inputId,
            label = label
          )
        ),
        tags$i(
          class = "fa fa-angle-left pull-right"
        )
      ),
      tags$ul(
        class = "treeview-menu",
        style = "display: none",
        `data-expanded` = inputId,
        ...
      )
    )
  }
  return(ui)
}

actionSubItem <- function(inputId, label) {
  ui <- tags$li(
    tags$a(
      `data-toggle` = "tab",
      `data-value` = "tab",
      tags$div(
        class = "div-btn-sidebar-sub",
        shiny::actionButton(
          inputId = inputId,
          label = tags$div(
            tags$i(
              class = "fa fa-angle-double-right"
            ),
            paste0(" ", label)
          )
        )
      )
    )
  )
}

multiple_actionItem <- function(inputId_list, label_list) {
  ui <- purrr::pmap(
    .l = list(
      inputId = inputId_list,
      label = label_list
    ),
    .f = actionItem
  )
}
