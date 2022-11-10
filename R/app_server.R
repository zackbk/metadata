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
                      O2 = sampleDT$MLFB, # O2Empty,
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
  
  #export button 
  
  output$export <- shiny::downloadHandler(
    filename = function() {
      "searchResults.html"
    },
    content = function(fName) {
      
      #create Rmarkdown file
      if(!file.exists("Export.Rmd"))
        writeLines('
 ---
 title: "Export"
 output: html_document
 params: 
    outputMain: "NA"
    outputParent: "NA"
 date: "`r Sys.Date()`"
 author: zackbk, metadata
 ---
 # Files
 ```{r include = TRUE, echo = FALSE}
 params$outputMain
  ```
  
 # Folders
 ```{r include = TRUE, echo = FALSE}
  params$outputParent
  ```','Export.Rmd')
      
      shinyWidgets::sendSweetAlert(session = session,title = "Saving Results", text = "")
      
      rmarkdown::render(input = "Export.Rmd",
                        output_file = fName,
                        output_format = "html_document",
                        params = list('outputMain' = r$MainView,
                                      'outputParent' = r$ParentView#,
                                      #'outputSummary' = DT::datatable(r$summaryView),
                                      #'outputSearchOptions' = r$searchOptions
                        ),
                        envir = new.env(parent = parent.frame())
      )
      
      shinyWidgets::closeSweetAlert(session = session)
    }
  )
  
}
