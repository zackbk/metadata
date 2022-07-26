#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  print("run app_server")
  r <- reactiveValues(temp = data.table::copy(O2Empty)[, filter := TRUE],
                      O2 = O2Empty, T2 = T2Empty, T3 = T3Empty,
                      WCA = character(0),timefield = character(0),
                      FName = character(0), drives = character(0)
                      )
  
 # callModule(mod_1_body_server, "1_body_ui_1", r)
  callModule(mod_4_engine_server,"4_engine_ui_1", r)
  callModule(mod_2_controller_server, "2_controller_ui_1", r)
  callModule(mod_2_2_subctlr_server, "2_2_subctlr_ui_1", r)
  callModule(mod_3_view_server, "3_view_ui_1", r)
  
}
