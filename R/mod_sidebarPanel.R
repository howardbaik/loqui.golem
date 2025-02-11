#' sidebarPanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sidebarPanel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(textInput(ns("email"), "Email Address (where video should be sent)"), style = "font-size: 18px"),
    div(
      shinyWidgets::prettySwitch(ns("auto_email"), "Once video finishes rendering, send email automatically",
                                 value = TRUE, status = "success", fill = TRUE),
      style = "color: #1c3b61;font-size:16px"
    ),
    div(
      h5("NOTE: For Automatic Emails, Keep Browser Open"),
      style = "color: #1c3b61;font-size:16px"
    ),
    div(
      radioButtons(ns("presentation_tool"), "Presentation Tool",
                   c("Google Slides" = "google_slides",
                     "Powerpoint" = "powerpoint")),
      style = "font-size:18px"
    ),
    div(
      shinyWidgets::prettySwitch(ns("burn_subtitle"), "Embed Subtitles",
                                 value = TRUE, status = "success", fill = TRUE),
      style = "color: #1c3b61;font-size:16px"
    ),
    uiOutput(ns("user_input")),  # FIXED: Added ns()
    div(
      shinyWidgets::pickerInput(ns("service"),
                                label = "Text-to-Speech Service",
                                choices = c("Coqui TTS" = "coqui")),
      style = "font-size:18px"
    ),
    uiOutput(ns("voice_options")),  # FIXED: Added ns()
    actionButton(ns("generate"), "Generate", icon = icon("person-running")),
    br(),
    br(),
    tags$img(src = "www/img/logo.png", width = "90%")
  )
}

#' Sidebar Panel Server Functions
#'
#' @noRd
mod_sidebarPanel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Show different inputs depending on Google Slides or PowerPoint
    output$user_input <- renderUI({
      if (input$presentation_tool == "google_slides") {
        div(
          textInput(ns("gs_url"),
                    label = "Google Slides URL (Enable Link Sharing)",
                    placeholder = "Paste a Google Slides URL"),
          style = "font-size:18px"
        )
      } else {
        fileInput(ns("pptx_file"), NULL, accept = ".pptx",
                  buttonLabel = "Upload .pptx")
      }
    })

    # Voice Options
    output$voice_options <- renderUI({
      if (input$service == "coqui") {
        div(
          selectInput(ns("coqui_model_name"), "Select Model Name (Voice)",
                      choices = c("tacotron2-DDC_ph", "jenny", "fast_pitch"),
                      selected = "jenny"),
          style = "font-size:18px"
        )
      }
    })





  })
}
