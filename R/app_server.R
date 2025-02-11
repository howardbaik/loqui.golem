#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  mod_titlePanel_server("titlePanel_1")
  mod_sidebarPanel_server("sidebarPanel_1")
  mod_mainPanel_server("mainPanel_1")
}
