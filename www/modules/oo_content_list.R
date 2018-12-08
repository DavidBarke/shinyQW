#' @export
content_list_R6 <- R6::R6Class(
  "content_list",
  public = list(
    initialize = function(id, sortable = FALSE) {
      private$container_id <- "list_container" %_% id
      private$sortable <- sortable
    },

    container = function() {
      ns <- private$ns
      if (!private$container_created) {
        private$container_created <- TRUE
        return(div(id = ns(private$container_id)))
      }
    },

    set_session = function(session) {
      private$session = session
    },

    add_element = function(content_element) {
      ns <- private$ns
      if (!content_element$get("id") %in% private$elements_id) {
        private$elements_id <- c(private$elements_id, content_element$get("id"))
        private$elements <- c(private$elements, content_element)
        insertUI(
          selector = paste0("#", ns(private$container_id)),
          where = "beforeEnd",
          ui = content_element$ui(),
          session = private$session
        )
      } else {
        element_container_id = content_element$get("container_id")
        shinyjs::show(
          selector = paste0("#", element_container_id)
        )
      }
    },

    add_element_actionButton = function(
      content_element, actionButton_id, actionButton_session
    ) {
      ns <- private$ns
      element_container_id <- content_element$get("container_id")
      observeEvent(
        actionButton_session$input[[actionButton_id]],
        handler.quoted = TRUE,
        handlerExpr = substitute(
          expr = {
            if (!content_element$get("id") %in% private$elements_id) {
              private$elements_id <- c(private$elements_id, content_element$get("id"))
              private$elements <- c(private$elements, content_element)
              insertUI(
                selector = paste0("#", ns(private$container_id)),
                where = "beforeEnd",
                ui = content_element$ui(),
                session = private$session
              )
            } else {
              shinyjs::show(
                selector = paste0("#", element_container_id)
              )
            }
          },
          env = list(
            element_container_id = element_container_id
          )
        )
      )
      # Make content list sortable, this piece of code is called every time a
      # new content element is added.
      if (private$sortable) {
        shinyjqui::jqui_sortable(
          paste0("#", ns(private$container_id))
        )
      }
    },

    append_tab = function(content_element_id, tab, select = FALSE) {
      content_element <- self$get_content_element_by_id(
        id = content_element_id
      )
      stopifnot("content_tabBox" %in% class(content_element))
      content_element$append_tab(
        tab = tab,
        select = select
      )
    },

    get_content_element_by_id = function(id) {
      stopifnot(id %in% private$elements_id)
      private$elements[[which(private$elements_id == id)]]
    },

    hide_element = function(content_element) {
      if (content_element$get("id") %in% private$element_id) {
        element_container_id <- content_element$get("container_id")
        shinyjs::hide(
          selector = paste0("#", element_container_id)
        )
      }
    }
  ),
  private = list(
    container_created = FALSE,
    container_id = NULL,
    sortable = FALSE,
    session = NULL,
    elements_id = list(),
    elements = list(),

    ns = function(id) {
      if (is.null(private$session)) {
        ns <- shiny::NS(NULL)
      } else {
        ns <- private$session$ns
      }
      ns(id)
    }
  )
)
