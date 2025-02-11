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
mod_sidebarPanel_server <- function(id, r) {
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

    # Create unique name for video file
    video_name <- eventReactive(input$generate, {
      current_time <- Sys.time()
      current_time <- format(current_time, "%Y-%m-%d-%H-%M-%S")
      unique_file_name <- paste0("www/ari-video-", current_time, ".mp4")

      unique_file_name
    })

    # Video with subtitles
    video_name_subtitle <- eventReactive(input$burn_subtitle, {
      # create unique name for video file
      current_time <- Sys.time()
      current_time <- format(current_time, "%Y-%m-%d-%H-%M-%S")
      unique_file_name <- paste0("www/subtitled-ari-video-", current_time, ".mp4")

      unique_file_name
    })

    # Generate video
    observeEvent(input$generate, {

      # Read inputs to be used inside future_promise()
      service <- input$service
      coqui_model_name <- input$coqui_model_name
      coqui_vocoder_name <- switch(coqui_model_name,
                                   "jenny" = "jenny",
                                   "tacotron2-DDC_ph" = "ljspeech/univnet",
                                   "fast_pitch" = "ljspeech/hifigan_v2",
                                   stop("Invalid model name"))

      which_tool <- input$presentation_tool
      burn_subtitle <- input$burn_subtitle
      gs_url <- input$gs_url
      pptx_upload_datapath <- input$pptx_file$datapath
      user_email <- input$email
      auto_email <- input$auto_email
      video_name <- video_name()
      video_name_subtitle <- video_name_subtitle()
      app_url <- "https://loqui.fredhutch.org"
      # download google slides as pptx
      if(which_tool == "google_slides") {
        pptx_path <- gsplyr::download(gs_url, type = "pptx")
      } else {
        # or fetch path to pptx on server
        pptx_path <- pptx_upload_datapath
      }
      pptx_notes_vector <- ptplyr::extract_notes(pptx_path)

      # download google slides as pdf
      if (which_tool == "google_slides") {
        pdf_path <- gsplyr::download(gs_url, type = "pdf")
      } else {
        # convert pptx slides to pdf
        if (Sys.info()['sysname'] == "Linux") {
          Sys.setenv(LD_LIBRARY_PATH="")
        }
        pdf_path <- ptplyr::convert_pptx_pdf(pptx_upload_datapath)
      }

      pdf_info <- pdftools::pdf_info(pdf = pdf_path)
      video_title <- pdf_info$keys$Title



      image_path <- ptplyr::convert_pdf_png(pdf_path)

      # ari_spin()----
      ari::ari_spin(images = image_path,
                    paragraphs = pptx_notes_vector,
                    output = video_name,
                    tts_engine_args = ari::coqui_args(coqui_model_name,
                                                      coqui_vocoder_name),
                    subtitles = TRUE)

      # Final output
      # Replace "www" with "i"
      if (burn_subtitle) {
        rendered_video_path <- gsub("www", "i", video_name_subtitle)
      } else {
        rendered_video_path <- gsub("www", "i", video_name)
      }

      final_res <- c(rendered_video_path, video_title)

      r(final_res)
    })
  })
}
