#' Store the session object of modules
#'
#' Use nodes to store the session objects of all modules in a shiny app in a tree-like
#' data-structure. A node object is an instantiation of the R6Class 'node'.
#'
#' Instantiate a new node with \code{node$new(name, parent, session)}. Passing the
#' arguments \code{name} and \code{session} is mandatory. The root or entry node to
#' a tree obviously has no parent.
#'
#' @usage
#' NULL
#'
#' @format
#' NULL
#'
#' @return
#' Public methods and fields are accesible using the '$' operator.
#' \item{add_child(child)}{Add a \code{child} node to private$children}
#' \item{child(path_to_child)}{Get a child of the node determined by the character
#' vector \code{path_to_child}. If there is a node in private$children with the name
#' of the first element of \code{path_to_child}, \code{child()} looks in the children
#' of this node for an child node with the name of the second element of
#' \code{path_to_child} and so on and returns the last child found. Calling this
#' method without arguments has the same effect as \code{children_names()}}
#' \item{children_names()}{Returns the names of all children nodes as a character
#' vector.}
#' \item{create_list()}{Returns a list representing all child and child-child nodes.}
#' \item{get(what)}{Returns the private field or method with the name \code{what}.}
#' \item{sibling(name)}{Returns the sibling node with name \code{name}. Calling this
#' method without arguments returns the names of all siblings.}
#'
#' The following private fields and methods which are accesible via
#' \code{get()}:
#' \item{children}{A list of nodes.}
#' \item{name}{A character string, which is unique within \code{sibling()}.}
#' \item{parent}{A parent node.}
#' \item{session}{Usually a shiny session object.}
#'
#' @examples
#' \dontrun{
#'   # Entry point of shiny app
#'   server <- function(input, output, session) {
#'     # root node has no parent
#'     self <- node$new("root", session = session)
#'
#'     shiny::callModule(module, "id_module", parent = self)
#'   }
#'
#'   # Module server function
#'   module <- function(input, output, session, parent) {
#'     self <- node$new("module", parent, session)
#'
#'     shiny::callModule(module, "id_module", parent = self)
#'   }
#'
#'   module_2 <- function(input, output, session, parent) {
#'     # Children can have the same name as their parent
#'     self <- node$new("module", parent, session)
#'   }
#' }
#'
#' # Instantiate nodes
#' root <- node$new("root", session = "session")
#' branch_1 <- node$new("branch_1", root, "session")
#' branch_2 <- node$new("branch_2", root, "session")
#' leave <- node$new("leave", branch_1, "session")
#'
#' # Returns leave node
#' root$child(c("branch_1", "leave"))
#'
#' # Returns branch_2 node
#' branch_1$sibling(branch_1$sibling()[[1]])
#' leave$get("parent")$sibling("branch_2")
#' leave$get("parent")$get("parent")$child("branch_2")
#'
#' @export
node <- R6::R6Class(
  "node",
  public = list(
    initialize = function(name, parent, session) {
      if (missing(name)) stop("argument with name 'name' is missing")
      if (missing(session)) stop("argument with name 'session' is missing")
      if (!missing(parent)) {
        if (!"node" %in% class(parent)) stop("parent has to be a node object")
        if (name %in% parent$children_names()) stop(paste0("parent node has already a child with name '", name, "'"))
        private$parent <- parent
        parent$add_child(self)
      }
      private$name <- name
      private$session <- session
    },
    get = function(what) {
      private[[what]]
    },
    add_child = function(child) {
      private$children <- c(private$children, child)
    },
    children_names = function() {
      names <- character(0)
      for (i in seq_along(private$children)) {
        names[[i]] <- private$children[[i]]$get("name")
      }
      names
    },
    child = function(path_to_child) {
      if (missing(path_to_child)) {
        return(self$children_names())
      }
      stopifnot(is.character(path_to_child))
      if (path_to_child[[1]] %in% self$children_names()) {
        if (length(path_to_child) == 1) {
          return(private$children[[which(self$children_names() == path_to_child[[1]])]])
        } else {
          return(private$children[[which(self$children_names() == path_to_child[[1]])]]$child(path_to_child[-1]))
        }
      } else {
        stop("there is no node corresponding to the given path_to_child")
      }
    },
    sibling = function(name) {
      if (missing(name)) {
        children_names <- private$parent$children_names()
        return(children_names[-which(children_names == private$name)])
      }
      sibling <- private$parent$child(name)
      if (is.null(sibling)) stop(paste0("there is no sibling node with name '", name, "'"))
      return(sibling)
    },
    create_list = function() {
      l <- list()
      for (child in private$children) {
        l[[child$get("name")]] <- child$create_list()
      }
      return(l)
    }
  ),
  private = list(
    name = NULL,
    parent = NULL,
    children = NULL,
    session = NULL
  )
)

#' @export
viewer <- R6::R6Class(
  "viewer",
  public = list(
    initialize = function(id, selected = NULL, title = "Viewer",
                          width = 6, height = NULL, side = c("left", "right")) {
      private$id <- id
      private$selected <- selected
      private$title <- title
      private$width <- width
      private$height <- height
      private$side <- match.arg(side)
    },
    appendTab = function(tab, select = FALSE) {
      shiny::appendTab(
        inputId = private$id,
        tab = tab,
        select = select,
        session = private$session
      )
      private$createActionButton(tab)
    },
    get = function(what) {
      private[[what]]
    },
    insertTab = function(tab, target, position = c("before", "after"),
                         select = FALSE) {
      shiny::insertTab(
        inputId = private$id,
        tab = tab,
        target = target,
        position = match.arg(position),
        select = select,
        session = private$session
      )
      private$createActionButton(tab)
    },
    prependTab = function(tab, select = FALSE) {
      shiny::prependTab(
        inputId = private$id,
        tab = tab,
        target = target,
        select = select,
        session = private$session
      )
      private$createActionButton(tab)
    },
    removeTab = function(target) {
      shiny::removeTab(
        inputId = private$id,
        target = target,
        session = private$session
      )
    },
    set_session = function(session) {
      private$session <- session
    },
    tabBox = function() {
      if (!private$once) {
        ui <- shinydashboard::tabBox(
          id = private$id,
          selected = private$selected,
          title = private$title,
          width = private$width,
          height = private$height,
          side = private$side
        )
        private$once <- TRUE
        return(ui)
      }
      else print("tabBox has been already created.")
    }
  ),
  private = list(
    id = NULL,
    selected = NULL,
    title = "Viewer",
    width = 6,
    height = NULL,
    side = "left",
    once = FALSE,
    session = NULL,
    tabCounter = 0,
    createActionButton = function(tab) {
      data_value <- tab$attribs[["data-value"]]
      private$tabCounter <- private$tabCounter + 1
      closeId <- private$id %_% private$tabCounter
      div_button <- div(
        class = "div-btn-close",
        shiny::actionButton(
          inputId = closeId,
          label = NULL,
          icon = icon("window-close")
        )
      )
      selector <- paste0("#", private$id, " li a[data-value=\"", data_value, "\"]")
      shiny::insertUI(
        selector = selector,
        where = "beforeEnd",
        ui = div_button
      )
      observeEvent(private$session$input[[closeId]], {
        self$removeTab(target = data_value)
      }, domain = private$session)
    }
  )
)
