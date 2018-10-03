# TODO: Excel-Skip und Excel-Range funktionieren nicht, da das reactive, das die
# inputWidgets erstellt immer wieder durchlaufen wird. Es müsste einmal statisch die
# Anzahl an inputWidgets erzeugt werden, falls sheet_counts() später einmal größer ist,
# werden dann dynamisch neue inputWidgets hinzugefügt

# Überlegung: alle Elemente unsichtbar erstellen und stückweise sichtbar machen

#' @export
tab_import_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = ns("file"),
        label = "Lade eine Datei hoch:",
        placeholder = "Klicke hier."
      ),
      uiOutput(
        outputId = ns("import_settings")
      )
    ),
    mainPanel(
      DT::dataTableOutput(
        outputId = ns("vorschau_data")
      )
    )
  )
}

#' @export
tab_import <- function(input, output, session, user_data_storage, permanent_data_storage, values,
                       parent, ...) {

  self <- node$new("import_module", parent, session)

  ns <- session$ns

  ## Generelles

  # Reactives

  file <- reactive({
    shiny::validate(need(input$file, message = FALSE))
    input$file
  })

  file_type <- reactive({
    path <- file()$datapath
    split_path <- str_split(path, pattern = "\\.")
    split_path[[1]][length(split_path[[1]])]
  })

  data_vorschau <- reactive({
    data <- list()
    type <- file_type()
    if (type == "xlsx" || type == "xls") {
      for (i in 1:sheet_counts()) {
        name_i <- input[[paste("name_data", i, sep = "_")]]
        range_i <- input[[paste("excel_range", i, sep = "_")]]
        skip_i <- input[[paste("excel_skip", i, sep = "_")]]
        if (name_i == "" || is.null(name_i)) {

        } else {
          if (range_i != "") {
            data[[name_i]] <- read_excel(
              path = file()$datapath,
              sheet = i,
              col_names = input$excel_colnames,
              skip = skip_i,
              range = range_i
            )
          } else {
            data[[name_i]] <- read_excel(
              path = file()$datapath,
              sheet = i,
              col_names = input$excel_colnames,
              skip = skip_i
            )
          }
        }
      }
    } else if (type == "csv") {
      data[[input$name_data_1]] <- read_csv2(
        file = file()$datapath
      )
    } else {
      data <- data.frame(Fehler = "Dieses Dateiformat wird nicht akzeptiert.")
    }
    return(data)
  })

  # deprecated (zur Erinnerung noch nicht gelöscht)
  data <- reactive({
    file()
    add_data()
    isolate({
      data <- list()
      type <- file_type()
      if (type == "xlsx" || type == "xls") {
        for (i in 1:sheet_counts()) {
          name <- input[[paste("name_data", i, sep = "_")]]
          if (name == "") {

          } else {
            data[[name]] <- read_excel(
              path = file()$datapath,
              sheet = i,
              col_names = TRUE
            )
          }
        }
      } else if (type == "csv") {
        data[[input$name_data_1]] <- read_csv2(
          file = file()$datapath
        )
      } else {
        data <- data.frame(Fehler = "Dieses Dateiformat wird nicht akzeptiert.")
      }
    })
    return(data)
  })

  # Observe Events



  # Output
  output$vorschau_data <- DT::renderDT({
    if (sheet_counts() > 1) {
      req(names(data_vorschau()))
      data_vorschau()[[which(names(data_vorschau()) == input$select_vorschau_sheet)]]
    } else {
      data_vorschau()[[1]]
    }
  })

  output$import_settings <- renderUI({
    import_settings_list <- switch(file_type(),
           "xlsx" = excel_settings(),
           "xls" = excel_settings(),
           "csv" = csv_settings(),
           "txt" = txt_settings())
  })

  ## Import Settings

  # excel

  excel_settings <- reactive({
    ui_list <- tagList(
      ui_excel_select_vorschau_sheet(),
      tabsetPanel(
        id = ns("tabset_excel_settings"),
        tabPanel(
          title = "Allgemein",
          excel_sheet_names(),
          checkboxInput(
            inputId = ns("excel_colnames"),
            label = "Erste Zeile enthält Spaltennamen",
            value = TRUE
          )
        ),
        tabPanel(
          title = "Erweitert",
          ui_excel_skip_rows(),
          ui_excel_range(),
          ui_excel_apply_range_and_skip_to_all_sheets()
        )
      ),
      hr(),
      fluidRow(
        column(
          width = 4,
          actionButton(
            inputId = ns("add_all_excel"),
            label = "Alle Sheets importieren."
          )
        ),
        column(
          width = 4,
          actionButton(
            inputId = ns("add_one_excel"),
            label = "Nur ein Sheet importieren:"
          )
        ),
        column(
          width = 4,
          selectInput(
            inputId = ns("add_which_excel_sheet"),
            choices = NULL,
            label = NULL
          )
        )
      )
    )
    return(ui_list)
  })

  sheet_counts <- reactive({
    length <- 1
    if (file_type() == "xlsx" || file_type() == "xls") {
      length <- length(excel_sheets(file()$datapath))
    }
    return(length)
  })

  excel_sheet_names <- reactive({
    ui_list <- list()
    datapath <- file()$datapath
    names <- current_excel_sheet_names()
    half_counts <- sheet_counts() / 2
    rows <- floor(half_counts)
    for (i in 1:rows) {
      j <- 2 * i - 1
      ui_list[[i]] <- fluidRow(
        column(
          width = 6,
          textInput(
            inputId = ns(paste("name_data", j, sep = "_")),
            label = paste("Name des ", j, ". Tabellenblattes"),
            value = names[[j]]
          )
        ),
        column(
          width = 6,
          textInput(
            inputId = ns(paste("name_data", j + 1, sep = "_")),
            label = paste("Name des ", j + 1, ". Tabellenblattes"),
            # value = excel_sheets(datapath)[[j + 1]]
            value = names[[j + 1]]
          )
        )
      )
    }
    if (rows - half_counts != 0) {
      k <- 2 * rows + 1
      ui_list[[rows + 1]] <- fluidRow(
        column(
          width = 6,
          textInput(
            inputId = ns(paste("name_data", k, sep = "_")),
            label = paste("Name des ", k, ". Tabellenblattes"),
            value = names[[k]]
          )
        )
      )
    }
    return(ui_list)
  })

  current_excel_sheet_names <- reactive({
    names <- c()
    for (i in 1:sheet_counts()) {
      names[i] <- input[[paste("name_data", i, sep = "_")]]
    }
    if(is.null(names)) {
      names <- excel_sheets(file()$datapath)
    }
    return(names)
  })

  observeEvent(current_excel_sheet_names(), {
    updateSelectInput(
      session = session,
      inputId = paste("add_which_excel_sheet"),
      choices = current_excel_sheet_names()
    )
  })

  ui_excel_select_vorschau_sheet <- reactive({
    ui_list <- list()
    if (is.null(input$select_vorschau_sheet)) {
      selected = current_excel_sheet_names()[[1]]
    } else {
      selected = input$select_vorschau_sheet
    }
    if (sheet_counts() > 1) {
      ui_element <- selectInput(
        inputId = ns("select_vorschau_sheet"),
        label = "Wähle das anzuzeigende Tabellenblatt aus:",
        choices = current_excel_sheet_names(),
        selected = selected
      )
      ui_list[[1]] <- ui_element
    }
    return(ui_list)
  })


  # Übersprungene Zeilen in jedem Sheet, nur erstes Element sichtbar, sichtbares
  # Element ändert sich basierend auf input$select_vorschau_sheet
  ui_excel_skip_rows <- reactive({
    ui_list <- list()
    for (i in 1:sheet_counts()) {
      inputId <- paste("excel_skip", i, sep = "_")
      if (is.null(input[[inputId]])) {
        first <- TRUE
        value <- 0
      } else {
        first <- FALSE
        value <- input[[inputId]]
      }
      ui_element <- numericInput(
        inputId = ns(inputId),
        label = paste("Übersprungene Zeilen im Sheet ", current_excel_sheet_names()[[i]]),
        value = value,
        min = 0
      )
      if (i > 1 && first) {
        ui_list[[i]] <- hidden(ui_element)
      } else {
        ui_list[[i]] <- ui_element
      }
    }
    return(ui_list)
  })

  observeEvent(input$select_vorschau_sheet, {
    new_i <- which(names(data_vorschau()) == input$select_vorschau_sheet)
    other_i <- which(1:sheet_counts() != new_i)
    pmap(.l = list(id = paste("excel_skip", other_i, sep = "_")), .f = hide)
    shinyjs::show(id = paste("excel_skip", new_i, sep = "_"))
  })


  ui_excel_range <- reactive({
    ui_list <- list()
    for (i in 1:sheet_counts()) {
      id_i <- paste("excel_range", i, sep = "_")
      ui_element <- textInput(
        inputId = ns(id_i),
        label = paste("Bereich für Sheet ", current_excel_sheet_names()[[i]], sep = ""),
        value = ""
      )
      if (i > 1) {
        ui_list[[i]] <- hidden(ui_element)
      } else {
        ui_list[[i]] <- ui_element
      }
    }
    return(ui_list)
  })

  observeEvent(input$select_vorschau_sheet, {
    new_i <- which(names(data_vorschau()) == input$select_vorschau_sheet)
    other_i <- which(1:sheet_counts() != new_i)
    pmap(.l = list(id = paste("excel_range", other_i, sep = "_")), .f = hide)
    shinyjs::show(id = paste("excel_range", new_i, sep = "_"))
  })

  ui_excel_apply_range_and_skip_to_all_sheets <- reactive({
    ui_list <- list()
    ui_list[[1]] <- fluidRow(
      ui_excel_apply_skip_to_all_sheets(),
      ui_excel_apply_range_to_all_sheets()
    )
    return(ui_list)
  })

  ui_excel_apply_range_to_all_sheets <- reactive({
    #req(input[[shown_skip_element()]])
    ui_element <- column(
      width = 6,
      actionButton(
        inputId = ns("apply_skip_to_all_sheets"),
        label = paste("Bereich", input[[shown_skip_element()]], " auf alle Sheets anwenden", sep = "")
      )
    )
    return(ui_element)
  })

  ui_excel_apply_skip_to_all_sheets <- reactive({
    #req(input[[shown_range_element()]])
    ui_element <- column(
      width = 6,
      actionButton(
        inputId = ns("apply_range_to_all_sheets"),
        label = paste(input[[shown_range_element()]], " Zeilen in allen Sheets überspringen")
      )
    )
    return(ui_element)
  })

  shown_skip_element <- reactive({
    return(paste("excel_skip", which(current_excel_sheet_names() == input$select_vorschau_sheet), seq = "_"))
  })

  shown_range_element <- reactive({
    return(paste("excel_range", which(current_excel_sheet_names() == input$select_vorschau_sheet), seq = "_"))
  })

  observeEvent(input$apply_skip_to_all_sheets, {
    i <- 1:sheet_counts()
    for (i in 1:sheet_counts()) {
      updateNumericInput(
        session,
        inputId = paste("excel_skip", i, sep = "_"),
        value = input[[shown_skip_element()]]
      )
    }
    #pwalk(.l = list(session = session,
    #               inputId = paste("excel_skip", i, sep = "_"),
    #               value = input[[shown_skip_element()]]),
    #     .f = updateNumericInput)
  })

  observeEvent(input$apply_range_to_all_sheets, {
    #pwalk(.l = list(session = session,
    #              inputId = paste("excel_range", i, sep = "_"),
    #              value = input[[shown_skip_element()]]),
    #    .f = updateTextInput)
  })

  # csv

  csv_settings <- reactive({
    ui_list <- tagList(

    )
    return(ui_list)
  })

  # txt

  txt_settings <- reactive({
    ui_list <- tagList(

    )
    return(ui_list)
  })

  ## Return Value

  observeEvent(input$add_all_excel, {
    data_excel <- list()
    for (i in 1:sheet_counts()) {
      name <- input[[paste("name_data", i, sep = "_")]]
      range_i <- input[[paste("excel_range", i, sep = "_")]]
      skip_i <- input[[paste("excel_skip", i, sep = "_")]]
      if (name == "") {

      } else {
        if (range_i != "") {
          data_excel[[name]] <- read_excel(
            path = file()$datapath,
            sheet = i,
            col_names = input$excel_colnames,
            skip = skip_i,
            range = range_i
          )
        } else {
          data_excel[[name]] <- read_excel(
            path = file()$datapath,
            sheet = i,
            col_names = input$excel_colnames,
            skip = skip_i
          )
        }
      }
    }
    data_save(data_excel)
  })

  observeEvent(input$add_one_excel, {
    i <- which(names(data_vorschau()) == input$add_which_excel_sheet)
    data_excel <- list()
    data_excel[[input$add_which_excel_sheet]] <- data_vorschau()[[i]]
    data_save(data_excel)
  })

  data_save <- reactiveVal()

  return_user_data_storage <- reactive({
    return(data_save())
  })

  return_permanent_data_storage <- reactive({
    return(NULL)
  })

  return_values <- reactive({
    return(NULL)
  })

  return_list <- reactive({
    return_list <- list(
      user_data_storage = return_user_data_storage(),
      permanent_data_storage = return_permanent_data_storage(),
      values = return_values()
    )
    return(return_list)
  })

  return(return_list)
}
