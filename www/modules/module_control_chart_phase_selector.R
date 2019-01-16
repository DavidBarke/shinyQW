control_chart_phase_selector_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        width = 8,
        uiOutput(
          outputId = ns("phase_selector")
        )
      ),
      column(
        width = 4,
        actionButton(
          inputId = ns("add_group"),
          label = label_lang(
            de = "Neue Gruppe",
            en = "Add group"
          )
        ),
        actionButton(
          inputId = ns("remove_group"),
          label = label_lang(
            de = "Entferne Gruppe",
            en = "Remove group"
          )
        )
      )
    )
  )
}

control_chart_phase_selector <- function(
  input, output, session, .data, .values, parent, phases, 
  unique_suffix = NULL, ...
) {
  
  self <- node$new(
    paste0("control_chart_phase_selector", unique_suffix),
    parent, session
  )
  
  ns <- session$ns
  
  output$phase_selector <- renderUI({
    phases <- phases()
    ui <- tagList()
    for (i in seq_along(phases)) {
      ui[[i]] <- fluidRow(
        column(
          width = 6,
          tags$label(
            phases[i]
          )
        ),
        column(
          width = 6,
          selectInput(
            inputId = ns("select_phase" %_% i),
            label = NULL,
            choices = label_lang_list(
              de = c("Vorlaufphase", "Operationsphase"),
              en = c("Trial phase", "Operation phase"),
              value = c("trial", "operation")
            ),
            selected = fallback(input[["select_phase" %_% i]], "trial")
          )
        )
      )
    }
    ui
  })
  
  trial_names <- reactive({
    phases <- phases()
    length_phases <- length(phases)
    trial_names <- character()
    for (i in seq_len(length_phases)) {
      if (input[["select_phase" %_% i]] == "trial") {
        trial_names <- c(trial_names, phases[i])
      }
    }
    trial_names
  })
  
  return(trial_names)
}