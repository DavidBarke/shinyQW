# TODO: Kommentieren

#' Create a container for ui
#'
#' Create a \code{div} that can be filled with ui from a shiny server function.
#'
#' Instantiate a new tabList with \code{tabList_R6$new(id)}.
#'
#' @usage NULL
#'
#' @format NULL
#'
#' @return
#' Public methods and fields are accesible using the '$' operator.
#' \item{add_list_item_by_actionButton(ui, inputId, session)}{Add \code{ui}
#' (preferable_created using the method \code{tabBox}) to the \code{container}
#' if the \code{\link[shiny]{actionButton}} with \code{inputId=inputId} is
#' clicked. \code{session} is the session in which the
#' \code{\link[shiny]{actionButton}} is defined. This session has to be existing
#' at the time this method is called.}
#' \item{container()}{Call this method anywhere in your ui to create the
#' container for your elements. Note that this method can only be called once.}
#' \item{set_session(session)}{Set the session to a shiny session object before
#' you do anything with the instance in a server function.}
#' \item{tabBox(..., id, selected = NULL, title = NULL, width = 12,
#' height = NULL, side = c("left", "right"), collapsible = TRUE,
#' closeable = TRUE)}{Create a \code{collapsible} and/or \code{closeable}
#' \code{\link[shinydashboard]{tabBox}}, which has to be passed as \code{ui} to
#' \code{add_list_item_by_actionButton} to work properly. See
#' \code{\link[shinydashboard]{tabBox}} for the other arguments.}
#'
#' @export
tabList_R6 <- R6::R6Class(
  classname = "tabList",
  public = list(
    initialize = function(id, sortable = FALSE) {
      private$id_ext <- id
      private$id_int <- shiny:::createUniqueId()
      private$sortable <- sortable
    },
    add_list_item_by_actionButton = function(
      ui, inputId, session = shiny::getDefaultReactiveDomain()
    ) {
      selector_id = "container" %_% inputId
      print(selector_id)
      observeEvent(
        session$input[[inputId]],
        handler.quoted = TRUE,
        handlerExpr = substitute(
          expr = {
            if (!inputId %in% private$open_id) {
              private$open_id <- c(private$open_id, inputId)
              insertUI(
                selector = paste0("#", private$id_int),
                where = "beforeEnd",
                ui = ui,
                session = private$session
              )
            } else {
              show(
                selector = paste0("#", selector_id)
              )
            }
          },
          env = list(
            selector_id = selector_id
          )
        )
      )
      if (private$sortable) {
        jqui_sortable(paste0("#", private$id_int))
      }
    },
    container = function() {
      if (!private$container_created) {
        private$container_created = TRUE
        return(div(id = private$id_int))
      }
    },
    set_session = function(session) {
      private$session = session
    },
    tabBox = function(
      ..., inputId, selected = NULL, title = NULL, width = 12, height = NULL,
      side = c("left", "right"), collapsible = TRUE, closeable = TRUE,
      tabPanel_list = NULL
    ) {
      side <- match.arg(side)
      if (!collapsible) {
        if (is.null(tabPanel_list)) {
          ui <- shinydashboard::tabBox(
            title = title,
            width = width,
            height = height,
            side = side,
            ...
          )
        } else {
          ui <- do.call(
            shinydashboard::tabBox,
            c(
              list(
                title = title,
                width = width,
                height = height,
                side = side
              ),
              tabPanel_list
            )
          )
        }
      } else {
        if (is.null(tabPanel_list)) {
          ui <- shinydashboard::box(
            title = title,
            collapsible = TRUE,
            width = width,
            height = height,
            shinydashboard::tabBox(
              width = 12,
              side = side,
              ...
            )
          )
        } else {
          ui <- shinydashboard::box(
            title = title,
            collapsible = TRUE,
            width = width,
            height = height,
            do.call(
              shinydashboard::tabBox,
              c(
                list(
                  width = 12,
                  side = side
                ),
                tabPanel_list
              )
            )
          )
        }
        shiny::tagAppendAttributes(
          tag = ui$children[[1]]$children[[2]]$children[[1]],
          class = "collapsible-tab-box"
        )
        if (closeable) {
          closeId <- "close" %_% inputId
          header_ui = ui$children[[1]]$children[[1]]$children
          header_ui[[3]] <- header_ui[[2]]
          header_ui[[2]] <- div(
            class = "div-btn-close div-btn-close-tablist",
            actionButton(
              inputId = closeId,
              label = NULL,
              icon = icon("window-close")
            )
          )
          ui$children[[1]]$children[[1]]$children <- header_ui
          div_id <- "container" %_% inputId
          if (private$sortable) {
            ui <- div(
              id = div_id,
              class = "ui-sortable-handle",
              ui
            )
          } else {
            ui <- div(
              id = div_id,
              ui
            )
          }
          observeEvent(private$session$input[[closeId]], {
            hide(
              selector = paste0("#", div_id)
            )
          })
        }
      }
      return(ui)
    }
  ),
  private = list(
    container_created = FALSE,
    id_int = NULL,
    id_ext = NULL,
    open_id = character(),
    session = NULL,
    sortable = FALSE,
    tab_counter = 0
  )
)
