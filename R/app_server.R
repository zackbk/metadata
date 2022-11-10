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
                      j = 1,
                      O2 = function(j = r$j) sampleDT[[j]], # O2Empty,
                      T2 = T2Empty, T3 = T3Empty,
                      WCA = character(0),timefield = character(0),
                      FName = character(0), drives = character(0)
  )
  tryCatch(expr = {
    base::writeLines(text = rmdFile,con = "Export.Rmd")
  }, error = function(e) {
    warning("cannot write html template")
    print(e)
  })
  
  
  # callModule(mod_1_body_server, "1_body_ui_1", r)
  callModule(mod_4_engine_server,"4_engine_ui_1", r)
  callModule(mod_2_search_server, "2_search_ui_1", r)
  callModule(mod_2_2_subctlr_server, "2_2_subctlr_ui_1", r)
  callModule(mod_3_view_server, "3_view_ui_1", r)
  callModule(mod_4_1_script_server,"4_1_script_ui_1", r)
  
}
