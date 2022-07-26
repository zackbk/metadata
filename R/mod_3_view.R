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
    shinymaterial::material_tabs(
      tabs = c(
        "I  Home" = "home_tab",
        "II Files" = "main_tab",
        "III SubFolders" = "parent_tab",
        "IV Summary" = "summary_tab",
        "V Chart" = "chart_tab"
      )
    ),
    # Define tab content
    shinymaterial::material_tab_content(
      tab_id = "home_tab",
      # mod_4_engine_ui("4_engine_ui_1")
    ),
    shinymaterial::material_tab_content(
      tab_id = "main_tab",
      DT::DTOutput(ns("Main"))
    ),
    shinymaterial::material_tab_content(
      tab_id = "parent_tab",
      DT::DTOutput(ns("Parent"))
    ),
    shinymaterial::material_tab_content(
      tab_id = "summary_tab",
      DT::DTOutput(ns("Summary"))
    ),
    shinymaterial::material_tab_content(
      tab_id = "chart_tab",
      # shiny::plotOutput(ns("Timeline"))
      shinymaterial::material_card(title = "chart",
                                   shiny::uiOutput(outputId = ns('Chart'))
      )
    ),
    shinyWidgets::downloadBttn(outputId = ns("export"),label = "export results", style = "material-flat", color = "default", size = "md", block = FALSE)
  )
  
}

#' 3_view Server Function
#'
#' @noRd 
mod_3_view_server <- function(input, output, session, r){
  ns <- session$ns
  print("Run mod_3")
  # callModule(mod_4_engine_server,"4_engine_ui_1", r)

  output$Main <- DT::renderDT({
    r$MainView <- DT::datatable(
      ColView(r$temp[filter == TRUE, ], r),
      # extensions = 'Buttons',
      options = list(
        #dom = 'Bfrtip',
        # buttons = list(
        #   list(
        #     extend = "copy", 
        #     text = "COPY", 
        #     title = NULL
        #   )
        # ),
        sDom  = '<"top">flrt<"bottom">ip'),
      escape = FALSE ) %>% DT::formatStyle('ext',
                                           target = 'row',
                                           backgroundColor = DT::styleEqual(fileIcons$ext, fileIcons$bg_clr )) 
    # %>% DT::formatStyle('ext', target = 'cell',background = DT::styleEqual(fileIcons$ext, fileIcons$ico ))
    r$MainView
  })
  
  output$Parent <- DT::renderDT({ 
    r$ParentView <- DT::datatable( 
      ColView(r$temp[filter == TRUE, 
                     lapply(.SD,function(x) sum(x,na.rm=T)),
                     by = c("parentName","parentID","Level"),
                     .SDcols = c("TotalByteSize","TotalFileCount")], r, other = c('link','Owner','DateAccessed','Level','TotalByteSize','TotalFileCount') ) ,
      options = list(sDom  = '<"top">flrt<"bottom">ip'), # 'f' is the filter.
      escape = FALSE)
    r$ParentView
  })
  
  output$Summary <- DT::renderDT({
    r$summaryView 
  })
  
  output$Chart <- shiny::renderUI({  
    r$ChartView <- lapply(r$chartPkg,
                          function(x) {
                            if(x %in% "ggplot2"){
                              shiny::renderPlot(chartView(r, chartType = r$chartType, chartPkg = x))
                            } else if (x %in% "ggiraph") {
                              ggiraph::renderggiraph(chartView(r, chartType = r$chartType, chartPkg = x))
                            } else if (x %in% "plotly") {
                              plotly::renderPlotly(chartView(r, chartType = r$chartType, chartPkg = x))
                            } else {
                              htmltools::as.tags(chartView(r, chartType = r$chartType, chartPkg = x))
                            }
                          }
    )
    
    tagList(r$ChartView)
  })
  
  
  output$export <- shiny::downloadHandler(
    filename = function() {
      "searchResults.html"
    },
    content = function(fName) {
      # fName <- gsub("\\\\","/",tempfile())
      # DT::saveWidget(r$MainView,file = fName)
      # shinyalert::shinyalert(title = "Result(s) saved to", text = exportFile)
      
      rmarkdown::render(input = "R/Export.Rmd",
                        output_file = fName,
                        output_format = "html_document",
                        params = list('outputMain' = r$MainView,
                                      'outputParent' = r$ParentView,
                                      'outputSummary' = DT::datatable(r$summaryView),
                                      'outputSearchOptions' = r$searchOptions),
                        envir = new.env(parent = parent.frame())
      )
    }
  )
  
  
  
  # shinymaterial::material_spinner_show(session = session,output_id = ns('Timeline'))
  # p <- ggplot2::ggplot(r$temp[filter == TRUE, ])
  # p <- p + ggplot2::geom_point(ggplot2::aes(x = DateCreated, y = TotalFileCount, fill = TotalByteSize))
  # diff_r <- max(r$temp[filter == TRUE, DateCreated]) - min(r$temp[filter == TRUE, DateCreated])
  # print("diff_r")
  # print(diff_r)
  # if(diff_r < 1){
  #   p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m %d %H:%M:%S"))
  # } else if(diff_r < 10){
  #   p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m %d %H"))
  # } else if(diff_r < 100){
  #   p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m %d"))
  # } else if(diff_r < 1000){
  #   p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y %m"))
  # } else{
  #   p <- p + ggplot2::scale_x_datetime(labels = scales::date_format("%y"))
  # }
  # shinymaterial::material_spinner_hide(session = session,output_id = ns('Timeline'))
  # p 
  
  print("End mod_3")
}

## To be copied in the UI
# 

## To be copied in the server
# 

