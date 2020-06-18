#' 1_body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_body_ui <- function(id){
  ns <- NS(id)
  tagList(
    pageWithSidebar(headerPanel = headerPanel("Search OGAED Drive"),
                    sidebarPanel = sidebarPanel(
                      mod_2_controller_ui("2_controller_ui_1"),
                      width = 2
                    ),
                    mainPanel =  mainPanel(
                      mod_3_view_ui("3_view_ui_1") 
                    )
    )
  )
}
    
#' 1_body Server Function
#'
#' @noRd 
mod_1_body_server <- function(input, output, session, r){
  ns <- session$ns
  
  callModule(mod_2_controller_server, "2_controller_ui_1", r)
  callModule(mod_3_view_server, "3_view_ui_1", r)
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
