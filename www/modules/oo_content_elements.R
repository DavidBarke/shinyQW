content_proto_R6 <- R6::R6Class(
  "content_proto",
  public = list(
    initialize = function(id) {
      private$id <- id
      private$container_id <- "element_container" %_% id
      private$session <- shiny::getDefaultReactiveDomain()
    },

    get = function(what) {
      private[[what]]
    }
  ),
  private = list(
    id = NULL,
    container_id = NULL,
    session = NULL
  )
)

content_tabBox <- function(
  id, title, ..., footer = NULL, closeable = TRUE,
  collapsible = TRUE, width = 12, height = NULL,
  tabPanel_list = NULL
) {
  tabBox <- content_tabBox_R6$new(
    id = id,
    ... = ...,
    footer = footer,
    closeable = closeable,
    collapsible = collapsible,
    title = title,
    width = width,
    height = height,
    tabPanel_list = tabPanel_list
  )
  invisible(tabBox)
}

content_tabBox_R6 <- R6::R6Class(
  "content_tabBox",
  inherit = content_proto_R6,
  public = list(
    initialize = function(
      id, title, ..., footer = NULL, closeable = TRUE,
      collapsible = TRUE, width = 12, height = NULL,
      tabPanel_list = NULL
    ) {
      super$initialize(id)
      private$footer <- footer
      private$closeable <- closeable
      private$collapsible <- collapsible
      private$title <- title
      private$width <- width
      private$height <- height
      if (is.null(tabPanel_list)) {
        private$tabPanel_list <- list(...)
      } else {
        private$tabPanel_list <- tabPanel_list
      }
      invisible(self)
    },

    append_tab = function(tab, select = FALSE) {
      shiny::appendTab(
        inputId = private$id,
        tab = tab,
        select = select,
        session = private$session
      )
    },

    ui = function() {
      ui <- shinydashboard::box(
        title = private$title,
        collapsible = private$collapsible,
        width = private$width,
        height = private$height,
        do.call(
          shinydashboard::tabBox,
          c(
            list(
              id = private$id,
              width = 12
            ),
            private$tabPanel_list
          )
        ),
        footer = private$footer
      )
      if (private$closeable) {
        # Add close button
        closeId <- "close" %_% private$id
        box_header_ui <- ui$children[[1]]$children[[1]]$children
        box_header_ui[[3]] <- box_header_ui[[2]]
        box_header_ui[[2]] <- div(
          class = "div-btn-close div-btn-close-tablist",
          shiny::actionButton(
            inputId = closeId,
            label = NULL,
            icon = icon("times"),
            style = "color: #97a0b3"
          )
        )
        ui$children[[1]]$children[[1]]$children <- box_header_ui
        observeEvent(private$session$input[[closeId]], {
          shinyjs::hide(
            selector = paste0("#", private$session$ns(private$container_id))
          )
        })
      }
      ui <- div(
        id = private$container_id,
        class = "ui-sortable-handle",
        ui
      )
    },
    
    update_tab = function(selected) {
      updateTabsetPanel(
        session = private$session,
        inputId = private$id,
        selected = selected
      )
    }
  ),
  private = list(
    footer = NULL,
    closeable = TRUE,
    collapsible = TRUE,
    tabPanel_list = list(),
    title = NULL,
    width = 12,
    height = NULL
  )
)

content_dialog_R6 <- R6::R6Class(
  "content_dialog",
  inherit = content_proto_R6,
  public = list(
    initialize = function(
      id, ..., footer = NULL, closeable = TRUE,
      collapsible = TRUE, title = NULL, width = 12, height = NULL,
      allowed = TRUE,
      tabPanel_list = NULL
    ) {
      super$initialize(id)
      private$footer <- footer
      private$closeable <- closeable
      private$collapsible <- collapsible
      private$title <- title
      private$width <- width
      private$height <- height
      if (is.null(tabPanel_list)) {
        private$tabPanel_list <- list(...)
      } else {
        private$tabPanel_list <- tabPanel_list
      }
      private$tab_data <- tibble(
        value = purrr::map_chr(
          .x = private$tabPanel_list,
          .f = function(x) {
            x$attribs$`data-value`
          }
        ),
        allowed = allowed
      )
      invisible(self)
    },

    ui = function() {
      ui <- shinydashboard::box(
        title = private$title,
        collapsible = private$collapsible,
        width = private$width,
        height = private$height,
        tags$span(
          class = "span-btn-move",
          shiny::actionButton(
            inputId = private$id %_% "move_left",
            label = NULL,
            icon = shiny::icon("angle-left")
          ),
          shiny::actionButton(
            inputId = private$id %_% "move_right",
            label = NULL,
            icon = shiny::icon("angle-right")
          )
        ),
        do.call(
          shinydashboard::tabBox,
          c(
            list(
              id = private$id,
              width = 12
            ),
            private$tabPanel_list
          )
        ),
        footer = private$footer
      )
      observeEvent(private$session$input[[private$id %_% "move_left"]], {
        open_tab_value <- private$session$input[[private$id]]
        index <- which(private$tab_data$value == open_tab_value)
        if (index > 1 && TRUE) {
          shinyjs::addClass(
            class = "disabled-anchor",
            selector = paste0("#", private$session$ns(private$id), " li a")
          )
          shinyjs::removeClass(
            class = "disabled-anchor",
            selector = paste0("#", private$session$ns(private$id),
                              " li a[data-value=\"",
                              private$tab_data$value[index - 1], "\"]")
          )
          updateTabsetPanel(
            session = private$session,
            inputId = private$id,
            selected = private$tab_data$value[index - 1]
          )
        }
      })
      observeEvent(private$session$input[[private$id %_% "move_right"]], {
        open_tab_value <- private$session$input[[private$id]]
        index <- which(private$tab_data$value == open_tab_value)
        if (index < nrow(private$tab_data)) {
          shinyjs::addClass(
            class = "disabled-anchor",
            selector = paste0("#", private$session$ns(private$id), " li a")
          )
          shinyjs::removeClass(
            class = "disabled-anchor",
            selector = paste0("#", private$session$ns(private$id),
                              " li a[data-value=\"",
                              private$tab_data$value[index + 1], "\"]")
          )
          updateTabsetPanel(
            session = private$session,
            inputId = private$id,
            selected = private$tab_data$value[index + 1]
          )
        }
      })
      if (private$closeable) {
        # Add close button
        closeId <- "close" %_% private$id
        box_header_ui <- ui$children[[1]]$children[[1]]$children
        box_header_ui[[3]] <- box_header_ui[[2]]
        box_header_ui[[2]] <- div(
          class = "div-btn-close div-btn-close-tablist",
          shiny::actionButton(
            inputId = closeId,
            label = NULL,
            icon = icon("times"),
            style = "color: #97a0b3"
          )
        )
        ui$children[[1]]$children[[1]]$children <- box_header_ui
        observeEvent(private$session$input[[closeId]], {
          shinyjs::hide(
            selector = paste0("#", private$session$ns(private$container_id))
          )
        })
      }
      ui <- div(
        id = private$container_id,
        class = "ui-sortable-handle",
        ui
      )
    }
  ),
  private = list(
    footer = NULL,
    closeable = TRUE,
    collapsible = TRUE,
    tabPanel_list = list(),
    title = NULL,
    width = 12,
    height = NULL,
    tab_data = NULL
  )
)
