source("R\mod_4_engine.R")
if (interactive()) {
  
  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        mod_4_engine_ui("4_engine_ui_1")
      ),
      mainPanel(

      )
    )
  )
  
  server <- function(input, output) {
    
    r <- reactiveValues(temp = data.table::copy(O2Empty)[, filter := TRUE],
                        O2 = O2Empty)
    
    callModule(mod_4_engine_server,"4_engine_ui_1", r)
    
    
  }
  
  shinyApp(ui, server)
}