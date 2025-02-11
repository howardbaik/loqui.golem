#' titlePanel UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_titlePanel_ui <- function(id) {
  ns <- NS(id)
  tagList(
    img(src = "www/img/logo-loqui.jpeg", height = "45px"),
    "Loqui: A Shiny app for Creating Automated Videos",
    span(
      actionButton("demo",
                   label = "Demo",
                   icon = icon("youtube"),
                   onclick ="window.open(`https://youtu.be/G7JyvCAxg40`, '_blank')"),
      actionButton("help",
                   label = "Help",
                   icon = icon("circle-exclamation"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/loqui#getting-help`, '_blank')"),
      actionButton("github",
                   label = "Code",
                   icon = icon("github"),
                   width = "77px",
                   onclick ="window.open(`https://github.com/FredHutch/loqui`, '_blank')"),
      style = "position:absolute;right:2em;"
    )
  )
}

#' titlePanel Server Functions
#'
#' @noRd
mod_titlePanel_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
  })
}

## To be copied in the UI

## To be copied in the server
