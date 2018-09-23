#' @export
custom_tabsetPanel <- function(..., id = NULL, selected = NULL, type = c("tabs", "pills"), position = NULL) {
  tabsetHTML <- tabsetPanel(... = ..., id = id, selected = selected, type = type, position = position)
  print(tabsetHTML)
  list_li <- tabsetHTML$children[[1]]$children[[1]]
  print(list_li)
  for (i in seq_along(list_li)) {
    li <- list_li[[i]]
    href <- li$children[[1]]$attribs$href
    new_li <- shiny::tagAppendChild(li, actionButton(inputId = paste("Action", href), label = "Action"))
    tabsetHTML$children[[1]]$children[[1]][[i]] <- new_li
  }
  return(tabsetHTML)
}

#' @export
custom_appendTab <- function(inputId, tab, select = FALSE, menuName = NULL, session = shiny::getDefaultReactiveDomain(),
                             action_button, action_button_session = shiny::getDefaultReactiveDomain()) {
  shiny::appendTab(inputId, tab, select, menuName, session)
  ui_element <- action_button
  inputId <- session$ns(inputId)
  insertUI(
    selector = paste0("#", inputId, " li:last"),
    where = "beforeEnd",
    ui = ui_element,
    session = action_button_session
  )
}

#' @export
custom_prependTab <- function(inputId, tab, select = FALSE, menuName = NULL, session = shiny::getDefaultReactiveDomain(),
                              action_button, action_button_session = shiny::getDefaultReactiveDomain()) {
  shiny::prependTab(inputId, tab, select, menuName, session)
  ui_element <- action_button
  inputId <- session$ns(inputId)
  shiny::insertUI(
    selector = paste0("#", inputId, " li:first"),
    where = "beforeEnd",
    ui = ui_element,
    session = action_button_session
  )
}
