#' @export
menubar_ui <- function(id, title) {
  ns <- NS(id)
  full_box(
    title = title,
    width = 12,
    collapsible = TRUE,
    box("Box_1"),
    box("Box_2")
  )
}

#' @export
menubar <- function(input, output, session, user_data_storage,
                    permanent_data_storage, values, parent, ...) {
}
