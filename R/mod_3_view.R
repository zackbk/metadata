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
    shinymaterial::material_row(
      mod_2_controller_ui("2_controller_ui_1"),
      shinymaterial::material_column(
        width = 2,
        # Controllers
        shinyWidgets::dropMenu( # mod_3_1_viewCtlr
          tag = shinyWidgets::actionBttn(inputId = ns("sortOptions"),"Sort", style = "material-flat", color = "default", size = "md", block = FALSE, icon = icon("sort")),
          shiny::selectInput(inputId = ns("colView"),label = "Show Columns",choices = names(O2Clean),
                             selected = c("link","Owner","ext","DateAccessed","TotalByteSize","parentName"), multiple = TRUE),
          shiny::selectInput(inputId = ns("arrangeBy"),label = "Arrange by",choices = c("",names(O2Clean)), multiple = FALSE),
          shinyWidgets::switchInput(inputId = ns('order'), onLabel = "Z-A",offLabel = "A-Z",value = FALSE)
        )
      ),
      shinymaterial::material_column(
        width = 2,
        shinyWidgets::dropMenu(
          tag = shinyWidgets::actionBttn(inputId = ns("summaryOptions"),"summary", style = "material-flat", size = "md", color = "default", block = FALSE, icon = icon("receipt")),
          shiny::selectInput(inputId = ns("xCol"), label = "X", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "ext", multiple = FALSE),
          shiny::selectInput(inputId = ns("yCol"), label = "Y", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "TotalByteSize"),
          shiny::selectInput(inputId = ns("yNumCols"), label = "Zs", choices = c(O2Names$cols_numeric), selected = "TotalFileCount", multiple = TRUE),
          shiny::selectInput(inputId = ns("groupBy"), label = "Group By", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "Level", multiple = TRUE),
          shiny::selectInput(inputId = ns("SDfun"), label = "summarise..", choices = c("sum","nothing","mean","max","min","median","length","quantile25","quantile50","quantile75","quantile95",'cumsum'), selected = "sum", multiple = FALSE),
          shiny::selectInput(inputId = ns("SDunit"), label = "unit", choices = c("actual","percent"), selected = "actual", multiple = FALSE)
        ) 
      ),
      shinymaterial::material_column(
        width = 2,
        shinyWidgets::dropMenu( # mod_3_1_viewCtlr
          tag = shinyWidgets::actionBttn(inputId = ns("chartOptions"),"chart", style = "material-flat", color = "default", size = "md", block = FALSE, icon = icon("chart-area")),
          shiny::selectInput(inputId = ns("chartType"),label = "Chart type",choices = c("auto","area","boxplot","bar","col","point","density","raster","histogram","line","pie"), selected="auto"),
          shiny::selectInput(inputId = ns("chartPkg"),label = "Chart package",choices = c("ggiraph","ggplot2","plotly","base"), selected="ggplot2",multiple = TRUE),
          shinyWidgets::noUiSliderInput(inputId = ns("height_svg"),label = "height",min = 3,max = 20,value = 6,step = 0.25),
          shinyWidgets::noUiSliderInput(inputId = ns("width_svg"),label = "width",min = 3,max = 20,value = 6,step = 0.25)
        )
      )
    ),
    shinymaterial::material_tabs(
      tabs = c(
        "Files" = "main_tab",
        "SubFolders" = "parent_tab",
        "Summary" = "summary_tab",
        "Chart" = "chart_tab"
      )
    ),
    # Define tab content
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
    shinyWidgets::downloadBttn(outputId = ns("export"),label = "export", style = "material-flat", color = "default", size = "md", block = FALSE)
  )
}

#' 3_view Server Function
#'
#' @noRd 
mod_3_view_server <- function(input, output, session, r){
  ns <- session$ns
  
  callModule(mod_2_controller_server, "2_controller_ui_1", r)
  
  observeEvent(c(input$colView),{
    print("colView")
    r$colView <- unlist(input$colView)
  }, ignoreInit = FALSE)
  
  shiny::observeEvent(input$height_svg, r$height_svg <- input$height_svg)
  shiny::observeEvent(input$width_svg, r$width_svg <- input$width_svg)
  shiny::observeEvent(input$chartPkg, r$chartPkg <- input$chartPkg)
  shiny::observeEvent(input$xCol, r$chartType <- input$chartType)
  shiny::observeEvent(input$xCol, r$xCol <- input$xCol)
  shiny::observeEvent(input$yCol, r$yCol <- input$yCol)
  shiny::observeEvent(input$groupBy, r$groupBy <- input$groupBy)
  shiny::observeEvent(input$SDfun, r$SDfun <- input$SDfun)
  shiny::observeEvent(input$SDunit, r$SDunit <- input$SDunit)
  
  shiny::observeEvent(c(input$xCol,input$yCol,input$yNumCols,input$SDfun,input$groupBy,input$SDunit),{
    for(i in names(input) ){
      r$summaryInput[[i]] <- unlist(input[[i]])
    }
  })
  
  shiny::observeEvent(c(input$xCol,input$yCol,input$yNumCols,input$SDfun,input$groupBy,input$SDunit,r$temp),{
    if(!is.null(r$summaryInput) ){
      r$summaryView <- summaryCtlr(ctlr = r$summaryInput, rvalue = r, DataTab = r$temp[filter == TRUE, ])
    }
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
  
  observeEvent(c(input$arrangeBy,input$order),{
    print("arrangeBy")
    r$temp <- SortView(r$temp,input)
  },ignoreInit = TRUE)
  
  print("names('input')")
  isolate(print(names(input)))
  
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
  
  
}

## To be copied in the UI
# 

## To be copied in the server
# 

