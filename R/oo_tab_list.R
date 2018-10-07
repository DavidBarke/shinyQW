# TODO: Kommentieren

#' Create a container for arbitrary ui
#'
#' @export
tabList_R6 <- R6::R6Class(
  classname = "tabList",
  public = list(
    initialize = function(id) {
      private$id_ext = id
      private$id_int = shiny:::createUniqueId()
    },
    add_list_item_by_actionButton = function(
      ui, inputId, session = shiny::getDefaultReactiveDomain()
    ) {
      observeEvent(session$input[[inputId]], {
        if (!inputId %in% private$open_id) {
          private$open_id <- c(private$open_id, inputId)
          insertUI(
            selector = paste0("#", private$id_int),
            where = "beforeEnd",
            ui = ui,
            session = private$session
          )
        }
      })
    },
    add_list_item_by_hand = function() {
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
      ..., id, selected = NULL, title = NULL, width = 12, height = NULL,
      side = c("left", "right"), collapsible = TRUE, closeable = TRUE
    ) {
      private$tab_counter <- private$tab_counter + 1
      side <- match.arg(side)
      if (!collapsible) {
        ui <- shinydashboard::tabBox(
          id = id,
          title = title,
          width = width,
          height = height,
          side = side,
          ...
        )
      } else {
        ui <- shinydashboard::box(
          title = title,
          collapsible = TRUE,
          width = width,
          height = height,
          shinydashboard::tabBox(
            id = id,
            width = 12,
            side = side,
            ...
          )
        )
        ui$children[[1]]$children[[2]]$children[[1]]$attribs$class <- paste(
          ui$children[[1]]$children[[2]]$children[[1]]$attribs$class,
          "collapsible-tab-box"
        )
        if (closeable) {
          closeId <- id %_% "close" %_% private$tab_counter
          header_ui = ui$children[[1]]$children[[1]]$children
          header_ui[[3]] <- header_ui[[2]]
          header_ui[[2]] <- div(
            class = "div-btn-close",
            actionButton(
              inputId = closeId,
              label = NULL,
              icon = icon("window-close")
            )
          )
          ui$children[[1]]$children[[1]]$children <- header_ui
          div_id <- "container" %_% id %_% private$tab
          ui <- div(
            id = div_id,
            ui
          )
          last_added <- private$open_id[length(private$open_id)]
          observeEvent(private$session$input[[closeId]], {
            removeUI(
              selector = paste0("#", div_id),
              session = private$session,
              multiple = TRUE
            )
            index <- which(private$open_id == last_added)
            private$open_id <- private$open_id[-index]
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
    tab_counter = 0
  )
)
