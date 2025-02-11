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

    # Validate email format
    is_valid_email <- function(x) {
      grepl("([_+a-z0-9-]+(\\.[_+a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,14}))", x)
    }

    # Email validation feedback
    observeEvent(input$email, {
      if (input$email != "" && !is_valid_email(input$email)) {
        shinyFeedback::showFeedbackWarning(ns("email"), "Invalid email. Please try again.")
      } else {
        shinyFeedback::hideFeedback(ns("email"))
      }
    })

    # Google Slides link validation feedback
    observeEvent(input$gs_url, {
      res <- try(gsplyr::download(input$gs_url, type = "pptx"), silent = TRUE)
      if (input$gs_url != "" && inherits(res, "try-error")) {
        shinyFeedback::showFeedbackWarning(ns("gs_url"), "Please set General access of the slides to 'Anyone with the link'.")
      } else {
        shinyFeedback::hideFeedback(ns("gs_url"))
      }
    })

    # UI for selecting Google Slides or PowerPoint
    output$user_input <- renderUI({
      if (input$presentation_tool == "google_slides") {
        div(
          textInput(ns("gs_url"), "Google Slides URL (Enable Link Sharing)", placeholder = "Paste a Google Slides URL"),
          style = "font-size:18px"
        )
      } else {
        fileInput(ns("pptx_file"), NULL, accept = ".pptx", buttonLabel = "Upload .pptx")
      }
    })

    # Voice options UI
    output$voice_options <- renderUI({
      if (input$service == "coqui") {
        div(selectInput(ns("coqui_model_name"), "Select Model Name (Voice)",
                        choices = c("tacotron2-DDC_ph", "jenny", "fast_pitch"),
                        selected = "jenny"),
            style = "font-size:18px")
      }
    })


    # Video generation
    observeEvent(input$generate, {

      # Read input values
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
      video_name <- paste0("www/ari-video-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".mp4")
      video_name_subtitle <- paste0("www/subtitled-ari-video-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), ".mp4")
      app_url <- "https://loqui.fredhutch.org"

      # Download or fetch PPTX
      pptx_path <- if (which_tool == "google_slides") gsplyr::download(gs_url, type = "pptx") else pptx_upload_datapath
      pptx_notes_vector <- ptplyr::extract_notes(pptx_path)

      # Download or convert PDF
      pdf_path <- if (which_tool == "google_slides") gsplyr::download(gs_url, type = "pdf") else {
        if (Sys.info()['sysname'] == "Linux") Sys.setenv(LD_LIBRARY_PATH = "")
        ptplyr::convert_pptx_pdf(pptx_upload_datapath)
      }

      # Extract video title
      video_title <- pdftools::pdf_info(pdf_path)$keys$Title

      # Convert PDF to images
      image_path <- ptplyr::convert_pdf_png(pdf_path)

      # Generate video using ari
      ari::ari_spin(
        images = image_path,
        paragraphs = pptx_notes_vector,
        output = video_name,
        tts_engine_args = ari::coqui_args(coqui_model_name, coqui_vocoder_name),
        subtitles = TRUE
      )

      # Burn subtitles if enabled
      if (burn_subtitle) {
        srt_file <- paste0(tools::file_path_sans_ext(video_name), ".srt")
        ari::ari_burn_subtitles(video_name, srt_file, video_name_subtitle)
      }

      # Send email if enabled
      if (auto_email) {
        video_name_processed <- if (burn_subtitle) gsub("www/", "", video_name_subtitle) else gsub("www/", "", video_name)
        video_link <- paste0(app_url, "/i/", video_name_processed)
        date_time <- blastula::add_readable_time()

        email <- blastula::compose_email(
          body = md(glue::glue(
            "Dear Loqui User,\n\nTo access the video, click here: [{video_link}]({video_link}).\n\n
            To download, click the three vertical dots and select 'Download'.\n\n
            Visit our website: [https://hutchdatascience.org](https://hutchdatascience.org).\n\n
            Contact: howardbaek@fredhutch.org or file a [GitHub issue](https://github.com/FredHutch/loqui/issues).\n\n
            Howard Baek"
          )),
          footer = md(glue::glue("Email automatically sent on {date_time}."))
        )

        email %>% blastula::smtp_send(
          from = "loqui-noreply@fredhutch.org",
          to = user_email,
          subject = "Video Generated by Loqui - Fred Hutch Data Science Lab",
          credentials = creds_anonymous(host = "mx.fhcrc.org", port = 25)
        )
      }

      # Log video details in Google Sheets
      ffmpeg_cmd <- paste0("-i ", if (burn_subtitle) video_name_subtitle else video_name, " 2>&1 | grep \"Duration\"")
      duration_raw <- system2("ffmpeg", ffmpeg_cmd, stdout = TRUE)
      video_duration <- sub("Duration: ([0-9:.]+)", "\\1", regmatches(duration_raw, regexpr("Duration: (\\d{2}:\\d{2}:\\d{2}\\.\\d{2})", duration_raw)))

      googlesheets4::gs4_auth(cache = ".secrets", email = "howardbaek.fh@gmail.com")
      googlesheets4::sheet_append(
        "https://docs.google.com/spreadsheets/d/1G_HTU-bv2k5txExP8EH3ScUfGqtW1P3syThD84Z-g9k/edit?usp=sharing",
        data.frame(date_time = blastula::add_readable_time(), video_duration = video_duration, email = user_email)
      )

      # Replace "www" with "i" for final output
      rendered_video_path <- if (burn_subtitle) gsub("www", "i", video_name_subtitle) else gsub("www", "i", video_name)

      # Render video UI
      output$video_ui <- renderUI({
        tags$video(src = rendered_video_path, type = "video/mp4", height = "480px", width = "854px", autoplay = TRUE, controls = TRUE)
      })

      # Show video title
      output$video_info <- renderUI({
        span(textOutput("video_title"), style = "font-weight: bold; font-size: 25px; color: #1c3b61")
        output$video_title <- renderText(video_title)
      })

      # Show download buttons
      output$video_btn <- renderUI({
        column(12,
               downloadButton(ns("download_btn"), "Download Video"),
               downloadButton(ns("download_subtitle_btn"), "Download Subtitles"),
               actionButton(ns("send_email"), "Email", icon = icon("inbox")),
               align = "left")
      })

      # Download handlers
      output$download_btn <- downloadHandler(
        filename = function() "loqui_video.mp4",
        content = function(file) {
          file.copy(if (burn_subtitle) video_name_subtitle else video_name, file)
        },
        contentType = "video/mp4"
      )

      output$download_subtitle_btn <- downloadHandler(
        filename = function() "loqui_video_subtitle.srt",
        content = function(file) {
          srt_file <- paste0(tools::file_path_sans_ext(video_name), ".srt")
          file.copy(srt_file, file)
        }
      )
    })




  })
}







mod_sidebarPanel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  })
}
