#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  r <- reactiveValues(temp = data.table::copy(O2Clean)[, filter := TRUE])
  
 callModule(mod_1_body_server, "1_body_ui_1", r)
  
  # output$Extensions <- DT::renderDT({
  #   T2
  # })
  
}
