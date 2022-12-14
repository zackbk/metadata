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
    shinydashboard::tabBox(
      width = '100hh',
      tabPanel("Files", DT::DTOutput(ns("Main"))), 
      # tabPanel("Summary",  DT::DTOutput(ns("Summary"))),
      tabPanel("Folders", DT::DTOutput(ns("Parent")))#,
      #tabPanel("Chart", shiny::uiOutput(outputId = ns('Chart')))
    )
    
  )
  
}

#' 3_view Server Function
#'
#' @noRd 
mod_3_view_server <- function(input, output, session, r){
  ns <- session$ns
  print("Run mod_3")
  
  
  output$Main <- DT::renderDT({
    print("main")
    if('ext' %in% names(r$temp)) {
      r$MainView <- DT::datatable(
        ColView(r$temp[filter == TRUE, ], r),
        options = list(scrollX = TRUE,
          sDom  = '<"top">flrt<"bottom">ip'
        ),filter = "top",
        escape = FALSE ) %>% DT::formatStyle('ext',
                                             target = 'row',
                                             backgroundColor = DT::styleEqual(fileIcons$ext, fileIcons$bg_clr ))   
    } else{
      r$MainView <- DT::datatable(
        ColView(r$temp[filter == TRUE, ], r),
        options = list(scrollX = TRUE,
          sDom  = '<"top">flrt<"bottom">ip'
        ),filter = "top",
        escape = FALSE )
    }
    print("end main")
    r$MainView
  })
  
  output$Parent <- DT::renderDT({ 
    print("parent")
    # if(sum(!c('parentName','parentID','Level','TotalByteSize','TotalFileCount') %in% names(r$temp)) == 0 ){
    
    byCols <- c("parentName","parentID","Level")
    byCols <- byCols[byCols %in% names(r$temp)]
    sdCols <- c("TotalByteSize","TotalFileCount","DateWritten")
    sdCols <- sdCols[sdCols %in% names(r$temp)]
    otherCols <- c('link','Owner','DateWritten','Level','TotalByteSize','TotalFileCount')
    otherCols <- otherCols[otherCols %in% names(r$temp)]
      r$ParentView <- DT::datatable( 
        ColView(r$temp[filter == TRUE, 
                       lapply(.SD,function(w) {
                         if( ! is.POSIXt(w) ){
                           sum(as.numeric(w),na.rm=T)
                         } else{
                           max(w,na.rm=T)
                         }
                         }),
                       by = c(byCols),
                       .SDcols = c(sdCols)]
                , r, other = otherCols ) ,
        options = list(sDom  = '<"top">flrt<"bottom">ip'),
        filter = "top", # 'f' is the filter.
        escape = FALSE)      
    # }
    print("end parent")
    r$ParentView
  })
  
  # output$Summary <- DT::renderDT({
  #   print("summaryView")
  #   r$summaryView 
  # })
  
  # output$Chart <- shiny::renderUI({
  #   print("ChartView")
  #   r$ChartView <- lapply(r$chartPkg,
  #                         function(x) {
  #                           if(x %in% "ggplot2"){
  #                             shiny::renderPlot(chartView(r, chartType = r$chartType, chartPkg = x))
  #                           } else if (x %in% "ggiraph") {
  #                             ggiraph::renderggiraph(chartView(r, chartType = r$chartType, chartPkg = x))
  #                           } else if (x %in% "plotly") {
  #                             plotly::renderPlotly(chartView(r, chartType = r$chartType, chartPkg = x))
  #                           } else {
  #                             htmltools::as.tags(chartView(r, chartType = r$chartType, chartPkg = x))
  #                           }
  #                         }
  #   )
  # 
  #   tagList(r$ChartView)
  # })
  # 
  
  print("End mod_3")
}

## To be copied in the UI
# 

## To be copied in the server
# 

