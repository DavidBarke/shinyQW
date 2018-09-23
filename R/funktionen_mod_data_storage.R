# TODO: observe der ActionButtons vernünftig implementieren

# MODUL: SELECT DATA ------------------------------------------------------------------------
#' @export
select_data_ui <- function(id) {
  ns <- NS(id)
  uiDivId <- id %_% "div"
  uiDivClass <- uiDivId %_% "class"
  div(
    id = ns(uiDivId),
    class = ns(uiDivClass),
    fluidRow(
      column(
        width = 6,
        uiOutput(
          outputId = ns("ui_select_data_type")
        )
      ),
      column(
        width = 6,
        uiOutput(
          outputId = ns("ui_select_data")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        uiOutput(
          outputId = ns("ui_select_data_column")
        )
      )
    ),
    interact_with_tabset_panel_ui(
      id = ns("id_interact_with_tabset_panel")
    )
  )
}

#' Select data for later use in other shiny modules.
#'
#' Select data stored in one or multiple \code{\link[shiny]{reactiveValues}}.
#'
#' @inheritParams interact_with_tabset_panel
#' @param tabset_data A tibble passed as \code{tabset_data} to
#' \code{\link{interact_with_tabset_panel}}.
#' @param select_column Deprecated. TODO: Remove
#' @param grid_select A \code{\link[tibble]{tibble}} specifying the displayed input
#' widgets for selecting the data. See 'Details'
#' @param block Number of input widgets displayed in a horizontal block.
#' @param parent A \code{\link{node}} object.
#' @param ... Currently not used.
#'
#' @return
#' A \code{\link[shiny]{reactiveValues}} containing information about the selection.
#' See 'Examples'.
#'
#' @details
#' \code{grid_select} has to be a \code{\link[tibble]{tibble}} with the following
#' columns:
#' \describe{
#'   \item{type}{A character specifing the type of the input widget. Either "select"
#'   for \code{\link[shiny]{selectInput}} or "selectize" for
#'   \code{\link[shiny]{selectizeInput}}}.
#'   \item{position}{A numeric. All input widgets are arranged by this in blocks
#'   of \code{block}.}
#'   \item{label}{The label for the input widget.}
#' }
#'
#' @examples
#'
#' @export
select_data <- function(input, output, session,
                        data_rvs = list(user_data_storage = user_data_storage,
                                    permanent_data_storage = permanent_data_storage),
                        values,
                        tabset_data = NULL, select_column = TRUE,
                        grid_select = tibble(type = "select", position = 1, label = "Select column"),
                        block = 2,
                        parent,
                        ...
                        )
{
  self <- node$new("select_data", parent, session)

  ns <- session$ns

  rvs <- reactiveValues()

  output$ui_select_data <- renderUI({
    ui_element <- selectInput(
      inputId = ns("select_data"),
      label = "Wähle Datensatz",
      choices = names(data_storage())
    )
    return(ui_element)
  })

  names_data_rvs <- reactive({
    if (is.list(data_rvs)) {
      stopifnot(!is.null(names(data_rvs)))
      return(names(data_rvs))
    } else {
      return(NULL)
    }
  })

  output$ui_select_data_type <- renderUI({
    selectInput(
      inputId = ns("data_type"),
      label = "Datentyp",
      choices = names_data_rvs()
    )
  })

  data_storage <- reactive({
    if (is.list(data_rvs)) {
      return(get(req(input$data_type), envir = list2env(data_rvs)))
    } else {
      return(data_rvs)
    }
  })

  grid <- reactive({
    grid <- grid_select
    grid <- grid %>%
      group_by(type) %>%
      mutate(count = 1:n()) %>%
      arrange(position)
    return(grid)
  })

  rvs_return_values <- reactiveValues()

  output$ui_select_data_column <- renderUI({
    grid <- grid()
    uiDivId_skeleton <- "div_select_data_column"
    inputId_skeleton <- "select_data_column"
    ui_list <- list()
    for (i in 1:nrow(grid)) {
      uiDivId <- uiDivId_skeleton %_% grid$type[[i]] %_% grid$count[[i]]
      inputId <- inputId_skeleton %_% i
      if (grid$type[[i]] == "select") {
        ui_element <- div(
          id = ns(uiDivId),
          selectInput(
            inputId = ns(inputId),
            label = grid_select$label[i],
            choices = names(data_storage()[[req(input$select_data)]])
          )
        )
      } else if (grid$type[[i]] == "selectize") {
        ui_element <- div(
          id = ns(uiDivId),
          selectizeInput(
            inputId = ns(inputId),
            label = grid_select$label[i],
            choices = names(data_storage()[[req(input$select_data)]]),
            multiple = TRUE
          )
        )
      }
      if (i %% 2 != 0) {
        ui_fluid_row <- list()
        ui_fluid_row[[1]] <- ui_element
        if (i == nrow(grid)) {
          ui_list[[(i + 1) / 2]] <- fluidRow(
            column(
              width = 6,
              ui_fluid_row[[1]]
            )
          )
        }
      } else {
        ui_fluid_row[[2]] <- ui_element
        ui_list[[i / 2]] <- fluidRow(
          column(
            width = 6,
            ui_fluid_row[[1]]
          ),
          column(
            width = 6,
            ui_fluid_row[[2]]
          )
        )
      }
      if (select_column == FALSE) {
        hide(
          selector = paste("#", ns(uiDivId))
        )
      }
    }
    rvs$return_values$start <- 1
    return(ui_list)
  })

  # CALL MODULES -------------------------------------------------------------

  call_interact_with_tabset_panel <- callModule(
    module = interact_with_tabset_panel,
    id = "id_interact_with_tabset_panel",
    data_rvs, values,
    tabset_data = tabset_data,
    parent = self,
    select_data = return_list
  )

  # RETURN ---------------------------------------------------------------------

  return_user_data_storage <- reactive({
    return(NULL)
  })

  return_permanent_data_storage <- reactive({
    return(NULL)
  })

  # TODO: An mehrere Selects anpassen
  return_values <- reactive({
    req(rvs$return_values$start)
    return_list <- list(
      data_type = req(input$data_type),
      data = list(
        "selected" = req(input$select_data),
        "column" = list()
      )
    )
    grid <- grid()
    for (i in 1:nrow(grid)) {
      name <- "selected" %_% i
      inputId <- "select_data_column" %_% i
      return_list$data$column[[name]] <- input[[inputId]]
    }
    return(return_list)
  })

  return_list <- reactive({
    return_list <- list(
      user_data_storage = return_user_data_storage(),
      permanent_data_storage = return_permanent_data_storage(),
      values = return_values()
    )
  })

  return(return_list)
}

#' @export
interact_with_tabset_panel_ui <- function(id) {
  ns <- NS(id)

  uiOutput(
    outputId = ns("ui")
  )
}

#' Interact with a shiny tabset panel
#'
#' Insert data into tabsetPanels accross different sessions within a modularized
#' shiny app using \code{\link[shiny]{actionButton}}.
#'
#' @param user_data_storage,permanent_data_storage,values
#' \code{\link[shiny]{reactiveValues}} storing data imported by the user, predefined
#' by the app author and other values.
#' @param tabset_data A tibble containing information about the involved tabsets and
#' actions. See 'Details'.
#' @param select_data \code{\link[shiny]{reactiveValues}} for example returned by
#' \code{\link{select_data}}.
#'
#' @export
interact_with_tabset_panel <- function(input, output, session,
                                       data_rvs, values,
                                       tabset_data, select_data,
                                       parent,
                                       ...) {
  self <- node$new("interact_with_tabset_panel", parent, session)

  ns <- session$ns

  rvs <- reactiveValues()

  output$ui <- renderUI({
    ui_element <- (process_tabset_data())$ui_element
    return(ui_element)
  })

  selected_data_storage <- reactive({
    if (is.list(data_rvs)) {
      get(select_data()$values$data_type, envir = list2env(data_rvs))
    } else {
      data_rvs
    }
  })

  sort_tabset_data <- reactive({
    data <- tabset_data
    data <- data %>%
      arrange(position)
    return(data)
  })

  process_tabset_data <- reactive({
    # Initialisiere tabset_counter
    if (is.null(rvs$tabset_counter)) {
      rvs$tabset_counter <- 1
    }
    data <- sort_tabset_data()
    ui_list <- list()
    for (i in seq_len(nrow(data))) {
      # Werte
      row <- data[i,]
      inputId <- row$type %_% row$id
      label <- row$label
      type <- row$type
      # UI-Element
      ui_element <- actionButton(
        inputId = ns(inputId),
        label = label
      )
      if (i %% 2 != 0) {
        ui_fluid_row <- list()
        ui_fluid_row[[1]] <- ui_element
        if (i == nrow(data)) {
          ui_list[[(i + 1) / 2]] <- fluidRow(
            column(
              width = 6,
              ui_fluid_row[[1]]
            )
          )
        }
      } else {
        ui_fluid_row[[2]] <- ui_element
        ui_list[[i / 2]] <- fluidRow(
          column(
            width = 6,
            ui_fluid_row[[1]]
          ),
          column(
            width = 6,
            ui_fluid_row[[2]]
          )
        )
      }
      rvs$tabset_data[[inputId]] <- 1
      # Observer
      if (is.null(rvs$observer[[inputId]]$initialized)) {
        # Stelle sicher, dass observeEvent nur einmal aufgerufen wird
        rvs$observer[[inputId]]$initialized <- TRUE
        observeEvent(event.quoted = TRUE,
                     eventExpr = substitute(input[[inputId]], list(inputId = inputId)),
                     handler.quoted = TRUE,
                     handlerExpr = substitute({
                       row <- sort_tabset_data()[1,]
                       data <- selected_data_storage()
                       selected <- req(select_data()$values$data$selected)
                       if (type == "append_new_tab") {
                         if (selected %in% names(rvs$open_tabs)) {
                           updateTabsetPanel(
                             session = row$session[[1]],
                             inputId = row$id,
                             selected = selected
                           )
                         } else {
                           rvs$open_tabs[[selected]] <- 1
                           closeTabId <- "close_tab" %_% selected
                           outputId <- "output" %_% selected %_% (rvs$tabset_counter + 1)
                           closeDataId <- "close_data" %_% selected %_% (rvs$tabset_counter + 1)
                           rvs$tabset_counter <- rvs$tabset_counter + 1
                           custom_appendTab(
                             inputId = row$id,
                             session = row$session[[1]],
                             tab = tabPanel(
                               title = selected,
                               fluidRow(
                                 column(
                                   width = 11,
                                   DT::dataTableOutput(
                                     outputId = ns(outputId)
                                   )
                                 ),
                                 column(
                                   width = 1,
                                   actionButton(
                                     inputId = ns(closeDataId),
                                     label = "X"
                                   )
                                 )
                               )
                             ),
                             select = TRUE,
                             action_button = actionButton(
                               inputId = withReactiveDomain(row$session[[1]], {
                                 ns(closeTabId)
                               }),
                               label = "X"
                             ),
                             action_button_session = row$session[[1]]
                           )
                           output[[outputId]] <- DT::renderDataTable({
                             return(datatable(data[[selected]]))
                           })
                           if (is.null(rvs$observer[[closeTabId]]$initialized)) {
                             rvs$observer[[closeTabId]]$initialized <- TRUE
                             observeEvent(input[[closeTabId]], {
                               removeTab(
                                 inputId = row$id,
                                 target = selected,
                                 session = row$session[[1]]
                               )
                               rvs$open_tabs <- rvs$open_tabs[-which(names(rvs$open_tabs) == selected)]
                               updateSelectInput(
                                 session = session,
                                 inputId = "select_data",
                                 selected = select_data()$values$data$selected
                               )
                             })
                           }
                           if (is.null(rvs$observer[[closeDataId]]$initialized)) {
                             rvs$observer[[closeDataId]]$initialized <- TRUE
                             observeEvent(input[[closeDataId]], {
                               shinyjs::hide(id = outputId)
                               shinyjs::hide(id = closeDataId)
                             })
                           }
                         }
                       } else if (type == "append_current_tab") {
                         active_panel <- row$session[[1]]$input[[row$id]]
                         outputId <- "output" %_% active_panel %_% (rvs$tabset_counter + 1)
                         closeDataId <- "close_data" %_% active_panel %_% (rvs$tabset_counter + 1)
                         rvs$tabset_counter <- rvs$tabset_counter + 1
                         ui_element <- fluidRow(
                           column(
                             width = 11,
                             DT::dataTableOutput(
                               outputId = ns(outputId)
                             )
                           ),
                           column(
                             width = 1,
                             actionButton(
                               inputId = ns(closeDataId),
                               label = "X"
                             )
                           )
                         )
                         selector <- paste0("#", row$session[[1]]$ns(row$id), " + div .active")
                         insertUI(
                           selector = selector,
                           where = "beforeEnd",
                           ui = ui_element
                         )
                         print(selected)
                         output[[outputId]] <- DT::renderDataTable({
                           return(datatable(data[[selected]]))
                         })
                         if (is.null(rvs$observer[[closeDataId]]$initialized)) {
                           rvs$observer[[closeDataId]]$initialized <- TRUE
                           observeEvent(input[[closeDataId]], {
                             shinyjs::hide(id = outputId)
                             shinyjs::hide(id = closeDataId)
                           })
                         }
                       }
                     }, list(type = type)))
      }
    }
    # Return
    return_list <- list(
      ui_element = ui_list
    )
    return(return_list)
  })
}
