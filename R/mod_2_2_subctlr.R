#' 2_2_subctlr UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_2_subctlr_ui <- function(id){
  ns <- NS(id)
  tagList(
    # shinymaterial::material_row(
    # shinymaterial::material_column(
    # 
    # shinydashboard::box(
    # shinyWidgets::dropMenu(
    # tag = shinyWidgets::actionBttn(inputId = ns("chartOptions"),"Table/Chart Settings", style = "material-flat", size = "md", color = "default", icon = icon("magnifying-glass-chart")),
    # shinyWidgets::dropdown( style = "unite", icon = icon("gear"), status = "warning", animate = animateOptions( enter = animations$fading_entrances$fadeInLeftBig, exit = animations$fading_exits$fadeOutRightBig),
    shinydashboard::box(width = 12, collapsible = FALSE,collapsed = FALSE,background = 'olive',
                        # shiny::tabPanel(title = "Table/Chart Settings",
                        shinyWidgets::pickerInput(inputId = ns("colView"),label = "Columns",choices = names(O2Empty),
                                                  selected = c("link","Owner","ext","DateAccessed","DateCreated","DateWritten","TotalByteSize","parentName"), multiple = TRUE)#,
                        # shinyWidgets::pickerInput(inputId = ns("xCol"), label = "X", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "Level", multiple = FALSE),
                        # shinyWidgets::pickerInput(inputId = ns("yCol"), label = "Y", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "TotalByteSize"),
                        # shinyWidgets::pickerInput(inputId = ns("yNumCols"), label = "Zs", choices = c(O2Names$cols_numeric), selected = "TotalFileCount", multiple = TRUE),
                        # shinyWidgets::pickerInput(inputId = ns("groupBy"), label = "Group (color) By", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "ext", multiple = TRUE),
                        # shinyWidgets::pickerInput(inputId = ns("SDfun"), label = "Summarise-by", choices = c("sum","nothing","mean","max","min","median","length","quantile25","quantile50","quantile75","quantile95",'cumsum'), selected = "sum", multiple = FALSE)
    )#,
    # shinydashboard::box(width = 12, collapsible = TRUE,title = "iv. Adv Chart",collapsed = TRUE,status = 'danger', solidHeader = TRUE,background = 'red',
    #                     # shiny::tabPanel(title = "Settings (cont'd)",icon = icon("gear"),
    #                     shinyWidgets::pickerInput(inputId = ns("SDunit"), label = "unit", choices = c("actual","percent"), selected = "actual", multiple = FALSE),
    #                     shinyWidgets::pickerInput(inputId = ns("chartType"),label = "Chart-type",choices = c("auto","area","boxplot","bar","col","point","density","raster","histogram","line","pie"), selected="col"),
    #                     shinyWidgets::pickerInput(inputId = ns("chartPkg"),label = "Chart-pkg",choices = c("ggiraph","ggplot2","plotly","base"), selected="ggplot2"),
    #                     shiny::numericInput(inputId = ns("height_svg"),label = "height",min = 3,max = 20,value = 6,step = 0.25, width = '100%'),
    #                     shiny::numericInput(inputId = ns("width_svg"),label = "width",min = 3,max = 20,value = 6,step = 0.25, width = '100%')
    # )
  )
  # )
  # )
  # )
}

#' 2_2_subctlr Server Functions
#'
#' @noRd 
mod_2_2_subctlr_server <-  function(input, output, session, r){
  ns <- session$ns
  print("RUN mod_2_2")
  
  
  # shiny::observeEvent(input$height_svg, r$height_svg <- input$height_svg, priority = 5)
  # shiny::observeEvent(input$width_svg, r$width_svg <- input$width_svg, priority = 5)
  # shiny::observeEvent(input$chartPkg, r$chartPkg <- input$chartPkg, priority = 5)
  # shiny::observeEvent(input$chartType, r$chartType <- input$chartType, priority = 5)
  # shiny::observeEvent(input$xCol, r$xCol <- input$xCol, priority = 5)
  # shiny::observeEvent(input$yCol, r$yCol <- input$yCol, priority = 5)
  # shiny::observeEvent(input$groupBy, r$groupBy <- input$groupBy, priority = 5)
  # shiny::observeEvent(input$SDfun, r$SDfun <- input$SDfun, priority = 5)
  # shiny::observeEvent(input$SDunit, r$SDunit <- input$SDunit, priority = 5)
  
  
  observeEvent(dim(r$O2),{
    print("Run r$O2")
    r$temp <- data.table::copy(r$O2)
    r$temp[, filter := TRUE]
	
    NewO2Names <- GetO2Names(r$O2)
    print("NewO2Names")
    print(NewO2Names)
    NewChoices <- names(r$O2)
    NewSelection <- GetSelection(input$colView, NewChoices)
    shinyWidgets::updatePickerInput(session = session, inputId = "colView",choices = NewChoices, selected = NewSelection)
    
    # NewChoices <- c(NewO2Names$cols_date,NewO2Names$cols_numeric,NewO2Names$cols_string)
    # NewSelection <- GetSelection(c('Level','make'),input$xCol, NewChoices)
    # shinyWidgets::updatePickerInput(session = session, inputId = "xCol", choices = NewChoices, selected = NewSelection[1])
    # 
    # NewSelection <- GetSelection(c('TotalByteSize','mpg'),input$yCol, NewChoices)
    # shinyWidgets::updatePickerInput(session = session, inputId = "yCol", choices = NewChoices, selected = NewSelection[1])
    # 
    # NewSelection <- GetSelection(c('ext','model'),input$groupBy, NewChoices)
    # shinyWidgets::updatePickerInput(session = session, inputId = "groupBy", choices = NewChoices, selected = NewSelection)
    # 
    # NewChoices <- c(NewO2Names$cols_numeric)
    # NewSelection <- GetSelection(c('TotalByteSize'),input$yNumCols, NewChoices)
    # 
    # shinyWidgets::updatePickerInput(session = session, inputId = "yNumCols", choices = NewChoices, selected = NewSelection)
    print("end r$O2")
  }, priority = 20,ignoreInit = FALSE)
  
  observeEvent(c(input$colView),{
    print("colView")
    r$colView <- unlist(input$colView)
  }, priority = 10, ignoreInit = FALSE)
  
  # shiny::observeEvent(c(input$xCol,input$yCol,input$yNumCols,input$SDfun,input$groupBy,input$SDunit),{
  #   print("run summaryInput")
  #   for(i in names(input) ){
  #     r$summaryInput[[i]] <- unlist(input[[i]])
  #   }
  # }, priority = 10)
  
  # shiny::observeEvent(c(input$xCol,input$yCol,input$yNumCols,input$SDfun,input$groupBy,input$SDunit,r$temp),{
  #   print("run summaryInput")
  #   if(!is.null(r$summaryInput) ){
  #     r$summaryView <- summaryCtlr(ctlr = r$summaryInput, rvalue = r, DataTab = r$temp[filter == TRUE, ])
  #   }
  # }, priority = 5)
  
  
  print("End mod_2_2")
}

## To be copied in the UI
# mod_2_2_subctlr_ui("2_2_subctlr_ui_1")

## To be copied in the server
# mod_2_2_subctlr_server("2_2_subctlr_ui_1", r)
