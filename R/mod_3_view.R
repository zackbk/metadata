#' 3_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_3_view_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(title = "Main", shinycssloaders::withSpinner(DT::DTOutput(ns("Main")))),
      tabPanel(title = "Parent", shinycssloaders::withSpinner(DT::DTOutput(ns("Parent")))),
      tabPanel(title = "Timeline", shinycssloaders::withSpinner(shiny::plotOutput(ns("Timeline")))) #,
      #tabPanel(title = "Summary", DT::DTOutput("Summary"))
    )
  )
}
    
#' 3_view Server Function
#'
#' @noRd 
mod_3_view_server <- function(input, output, session, r){
  ns <- session$ns
 
  output$Main <- DT::renderDT({
    r$temp[filter == TRUE, ]
  })
  
  output$Parent <- DT::renderDT({
    r$temp[filter == TRUE, 
           lapply(.SD,function(x) sum(x,na.rm=T)),
           by = c("parentName","parentID","Level"),
           .SDcols = c("TotalByteSize","TotalFileCount")]
  })
  
  output$Timeline <- shiny::renderPlot({
    p <- ggplot2::ggplot(r$temp[filter == TRUE, ])
    p <- p + ggplot2::geom_point(ggplot2::aes(x = DateCreated, y = TotalFileCount, fill = TotalByteSize))
    diff_r <- max(r$temp[filter == TRUE, DateCreated]) - min(r$temp[filter == TRUE, DateCreated])
    if(diff_r < 1){
      p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m %d %H:%M:%S"))
    } else if(diff_r < 10){
      p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m %d %H"))
    } else if(diff_r < 100){
      p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m %d"))
    } else if(diff_r < 1000){
      p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m"))
    } else{
      p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y"))
    }
    p
  })
  
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
