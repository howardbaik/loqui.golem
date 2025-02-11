#' mainPanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_mainPanel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(id = ns("inTabset"),
                tabPanel(
                  title = div("About",
                              style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                  value = ns("about"),
                  div(
                    uiOutput(ns("loqui_demo")),
                    h5("Privacy Policy: We only collect the date and time of usage, duration of the generated video, and the provided email address."),
                    h5("This initiative is funded by the following grant: National Cancer Institute (NCI) UE5 CA254170"),
                    style = "font-family: Arial; color: #1c3b61; font-size: 1.65rem")
                ),
                tabPanel(
                  title = div("Tips",
                              style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                  value = ns("tips")
                ),
                tabPanel(
                  title = div("Rendered Video",
                              style = "font-family: Arial; color: #1c3b61; font-weight: bold"),
                  value = ns("rendered_video"),
                  br(),
                  uiOutput(ns("video_ui")),
                  br(),
                  fluidRow(column(11, htmlOutput(ns("video_info")))),
                  fluidRow(uiOutput(ns("video_btn")))
                )
    )
  )
}

#' mainPanel Server Functions
#'
#' @noRd
mod_mainPanel_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Demo of Loqui
    output$loqui_demo <- renderUI({
      tags$video(src = "www/video/loqui.mp4",
                 type = "video/mp4",
                 height = "480px",
                 width = "790px",
                 controls = TRUE)
    })
  })
}


## To be copied in the server
#
