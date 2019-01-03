#' @export
`%_%` <- function(x, y) {
  paste(x, y, sep = "_")
}

#' Turn the first letter of a string in upper case
#'
#' @param x A character vector.
#'
#' @export
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}



# TODO: Nochmal nachdenken, ob die Funktion wirklich benötigt wird.
#' Append name-value pairs to a reactiveValues object
#'
#' Create default values for a \code{\link[shiny]{reactiveValues}} object.
#' This function covers the special case in which a module's server function has
#' been called but the ui isn't part of the DOM yet, so that input values are
#' still missing.
#'
#' @param rvs A \code{\link[shiny]{reactiveValues}} object.
#' @param ... \code{name = value} pairs (\code{rvs[[name]] <- value}).
#'
#' @export
append_to_reactiveValues <- function(rvs, ...) {
  dots <- list(...)
  observeEvent(
    eventExpr = 1,
    handlerExpr = {
      for (i in seq_along(dots)) {
        rvs[[names(dots[i])]] <- dots[[i]]
      }
    },
    once = TRUE
  )
}

# Zurzeit nicht verwendet, könnte möglicherweise nochmal von Nutzen sein
# Die inputIds der ui-Elemente setzen sich aus einem gemeinsamen Präfix (prefix_id) und einer
# einzigartigen Nummerierung zusammen
update_multiple_ui_elements <- function(session, input, prefix_id, update_function, length) {
  # Inputträger ist value
  if (update_function %in% c("updateNumericInput",
                             "updateColorInput",
                             "updateTextInput",
                             "updateCheckboxInput",
                             "updateDateInput",
                             "updateTextAreaInput")) {
    for (i in 1:length) {
      iInputId <- prefix_id %_% i
      do.call(
        what = update_function,
        args = list(
          session = session,
          inputId = iInputId,
          value = input[[iInputId]]
        )
      )
    }
  }
  # Inputträger ist selected
  if (update_function %in% c("updateSelectInput",
                             "updateSelectizeInput",
                             "updateCheckboxGroupInput",
                             "updateNavbarPage",
                             "updateNavlistPanel",
                             "updateTabsetPanel",
                             "updateRadioButtons",
                             "updateSliderInput")) {
    for (i in 1:length) {
      iInputId <- prefix_id %_% i
      do.call(
        what = update_function,
        args = list(
          session = session,
          inputId = iInputId,
          selected = input[[iInputId]]
        )
      )
    }
  }
}

call_multiple_modules <- function(module_vector, prefix = "call", infix = NULL,
                                  data, values,
                                  session_tree, parent) {
    for (module in module_vector) {
      name_reactive <- prefix %_% infix %_% module
      name_module <- infix %_% module
      id_module <- "id" %_% infix %_% module
      assign(name_reactive, callModule(
        module = eval(parse(text = name_module)),
        id = id_module,
        data = data,
        values = values,
        session_tree = session_tree,
        parent = parent
      ), envir = parent.frame())
    }
}

# TODO: Als Standardfunktion etablieren, bzw. shinyUtils-Package nutzen
# ACHTUNG: Das ist die aktuelle Funktion und nicht die in shinyUtils
#' @export
call_multiple_modules_2 <- function(module_templates,
                                    glue_module = list(x1 = "{template}"),
                                    glue_id = list(x1 = "id", x2 = "{template}"),
                                    glue_reactive = list(x1 = "call", x2 = "{template}"),
                                    ..., glue_list = NULL) {
  for (template in module_templates) {
    # Objektnamen erstellen
    for (type in c("reactive", "module", "id")) {
      glue_x <- get("glue" %_% type)
      x_list <- glue_x[stringr::str_detect(names(glue_x), "^x\\d+$")]
      x_list <- x_list[order(as.numeric(stringr::str_extract(names(x_list), "\\d+")))]
      if(is.null(glue_x$sep)) sep <- "_" else sep <- glue_x$sep
      expression_string <- paste(x_list, collapse = sep)
      assign("name" %_% type, glue::glue_data(.x = c(glue_list, list(template = template)), expression_string))
    }
    # Modul unter Verwendung der Objektnamen aufrufen
    module <- get(name_module)
    assign(name_reactive, shiny::callModule(
      module = get(name_module),
      id = name_id,
      ...
    ), envir = parent.frame())
  }
}

#' @export
include_markdown <- function(rmd_path) {
  render(
    rmd_path,
    html_fragment(),
    encoding = "UTF-8",
    quiet = TRUE
  )
  html_path <- stringr::str_replace(rmd_path, "(R|r)md$", "html")
  withMathJax(includeHTML(html_path))
}

create_subItems <- function(text, tabName) {
  ui <- do.call(shinydashboard::menuSubItem,
                list(text = text, tabName = tabName))
  return(ui)
}

#' Call multiple menuItems in a shinydashboard sidebarMenu
#'
#' @param struc Either a character vector for menuItems only or a named list
#' containing another named list if you want to generate menuItems with
#' menuSubItems. The names of the inner named list are interpreted as the
#' tabName whereas the content is interpreted as the text for menuSubItem.
#' @param menuItem_args Arguments passed to
#' \code{\link[shinydashboard:sidebarMenu]{menuItem}}.
#' @param menuSubItem_args Arguments passed to
#' \code{\link[shinydashboard:sidebarMenu]{menuSubItem}}.
#'
#' @export
multiple_menuItem <- function(struct, menuItem_args = NULL,
                              menuSubItem_args = NULL) {
  ui_item <- shiny::tagList()
  for (i in seq_along(struct)) {
    item <- struct[[i]]
    if (is.list(item) && length(item) != 0){
      ui_sub <- purrr::pmap(.l = list(text = names(item), tabName = item),
                            .f = create_subItems)
      ui_item[[i]] <- do.call(shinydashboard::menuItem, list(
        text = names(struct[i]),
        ui_sub
      ))
    } else {
      # NOCH NICHT GETESTET
      # stopifnot(is.character(struct))
      # ui_item[[i]] <- do.call(shinydashboard::menuItem, list(
      #   text = names(struct[i])
      # ))
    }
  }
  return(ui_item)
}


#' @export
label_lang <- function(...) {
  label_list <- list(...)
  label <- label_list[[get(".language", envir = parent.frame())]]
  return(label)
}

#' @export
label_lang_list <- function(..., value) {
  label <- label_lang(...)
  stopifnot(length(label) == length(value))
  label_list <- as.list(value)
  names(label_list) <- label
  label_list
}

#' @export
fallback <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else {
    return(x)
  }
}

maprange <- function(x, pmin, pmax, nmin, nmax) {
  nmin + ((x - pmin) / (pmax - pmin)) * (nmax - nmin)
}
