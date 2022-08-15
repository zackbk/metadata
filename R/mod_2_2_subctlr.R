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
        # width = 2,
        # Controllers
        shinyWidgets::dropMenu( # mod_3_1_viewCtlr
          tag = shinyWidgets::actionBttn(inputId = ns("sortOptions"),"Select/Sort", style = "material-flat", color = "default", size = "md", block = FALSE, icon = icon("sort")),
          shiny::selectInput(inputId = ns("colView"),label = "Show Columns",choices = names(O2Empty),
                             selected = c("link","Owner","ext","DateAccessed","DateCreated","DateWritten","TotalByteSize","parentName"), multiple = TRUE),
          shiny::selectInput(inputId = ns("arrangeBy"),label = "Arrange by",choices = c("",names(O2Empty)), multiple = FALSE),
          shinyWidgets::switchInput(inputId = ns('order'), onLabel = "Z-A",offLabel = "A-Z",value = FALSE)
        ),
      # ),
      # shinymaterial::material_column(
        # width = 2,
        shinyWidgets::dropMenu(
          tag = shinyWidgets::actionBttn(inputId = ns("chartOptions"),"Table/Chart", style = "material-flat", size = "md", color = "default", block = FALSE, icon = icon("magnifying-glass-chart")),
          shiny::selectInput(inputId = ns("xCol"), label = "X", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "ext", multiple = FALSE),
          shiny::selectInput(inputId = ns("yCol"), label = "Y", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "TotalByteSize"),
          shiny::selectInput(inputId = ns("yNumCols"), label = "Zs", choices = c(O2Names$cols_numeric), selected = "TotalFileCount", multiple = TRUE),
          shiny::selectInput(inputId = ns("groupBy"), label = "Group By", choices = c(O2Names$cols_numeric,O2Names$cols_string,O2Names$cols_date), selected = "Level", multiple = TRUE),
          shiny::selectInput(inputId = ns("SDfun"), label = "summarise..", choices = c("sum","nothing","mean","max","min","median","length","quantile25","quantile50","quantile75","quantile95",'cumsum'), selected = "sum", multiple = FALSE),
          shiny::selectInput(inputId = ns("SDunit"), label = "unit", choices = c("actual","percent"), selected = "actual", multiple = FALSE)
        ), 
      # ),
      # shinymaterial::material_column(
      #   width = 2,
        shinyWidgets::dropMenu( # mod_3_1_viewCtlr
          tag = shinyWidgets::actionBttn(inputId = ns("displayOptions"),"Display Options", style = "material-flat", color = "default", size = "md", block = FALSE, icon = icon("uncharted")),
          shiny::selectInput(inputId = ns("chartType"),label = "Chart type",choices = c("auto","area","boxplot","bar","col","point","density","raster","histogram","line","pie"), selected="auto"),
          shiny::selectInput(inputId = ns("chartPkg"),label = "Chart package",choices = c("ggiraph","ggplot2","plotly","base"), selected="ggplot2",multiple = TRUE),
          shinyWidgets::noUiSliderInput(inputId = ns("height_svg"),label = "height",min = 3,max = 20,value = 6,step = 0.25),
          shinyWidgets::noUiSliderInput(inputId = ns("width_svg"),label = "width",min = 3,max = 20,value = 6,step = 0.25)
        )
      # )
    # )
  )
}

#' 2_2_subctlr Server Functions
#'
#' @noRd 
mod_2_2_subctlr_server <-  function(input, output, session, r){
  ns <- session$ns
  print("RUN mod_2_2")
  
  
  shiny::observeEvent(input$height_svg, r$height_svg <- input$height_svg)
  shiny::observeEvent(input$width_svg, r$width_svg <- input$width_svg)
  shiny::observeEvent(input$chartPkg, r$chartPkg <- input$chartPkg)
  shiny::observeEvent(input$xCol, r$chartType <- input$chartType)
  shiny::observeEvent(input$xCol, r$xCol <- input$xCol)
  shiny::observeEvent(input$yCol, r$yCol <- input$yCol)
  shiny::observeEvent(input$groupBy, r$groupBy <- input$groupBy)
  shiny::observeEvent(input$SDfun, r$SDfun <- input$SDfun)
  shiny::observeEvent(input$SDunit, r$SDunit <- input$SDunit)
  
  
  observeEvent(dim(r$O2),{
    print("Run r$O2")
    
    GetSelection <- function(y,z = NewChoices) unlist(lapply(y,function(x) x[x %in% z]))
    
    NewO2Names <- GetO2Names(r$O2)
    
    NewChoices <- names(r$O2)
    NewSelection <- GetSelection(input$colView)
    shiny::updateSelectInput(session = session, inputId = ns("colView"),choices = NewChoices,selected = NewSelection)
    NewChoices <- c("",names(r$O2))
    NewSelection <- GetSelection(input$arrangeBy)
    shiny::updateSelectInput(session = session, inputId = ns("arrangeBy"),choices = NewChoices, selected = NewSelection)
    NewChoices <- c(NewO2Names$cols_numeric,NewO2Names$cols_string,NewO2Names$cols_date)
    NewSelection <- GetSelection(input$xCol)
    shiny::updateSelectInput(session = session, inputId = ns("xCol"), choices = NewChoices, selected = NewSelection)
    NewSelection <- GetSelection(input$yCol)
    shiny::updateSelectInput(session = session, inputId = ns("yCol"), choices = NewChoices, selected = NewSelection)
    NewSelection <- GetSelection(input$groupBy)
    shiny::updateSelectInput(session = session, inputId = ns("groupBy"), choices = NewChoices, selected = NewSelection)
    NewChoices <- c(NewO2Names$cols_numeric)
    NewSelection <- GetSelection(input$yNumCols)
    shiny::updateSelectInput(session = session, inputId = ns("yNumCols"), choices = NewChoices, selected = NewSelection)
    
  },ignoreInit = TRUE)
  
  observeEvent(c(input$colView),{
    print("colView")
    r$colView <- unlist(input$colView)
  }, ignoreInit = FALSE)
  
  shiny::observeEvent(c(input$xCol,input$yCol,input$yNumCols,input$SDfun,input$groupBy,input$SDunit),{
    for(i in names(input) ){
      r$summaryInput[[i]] <- unlist(input[[i]])
    }
  }, priority = 10)
  
  shiny::observeEvent(c(input$xCol,input$yCol,input$yNumCols,input$SDfun,input$groupBy,input$SDunit,r$temp),{
    if(!is.null(r$summaryInput) ){
      r$summaryView <- summaryCtlr(ctlr = r$summaryInput, rvalue = r, DataTab = r$temp[filter == TRUE, ])
    }
  }, priority = 5)
  
  observeEvent(c(input$arrangeBy,input$order),{
    print("arrangeBy")
    r$temp <- SortView(r$temp,input)
  },ignoreInit = TRUE)
  
  print("End mod_2_2")
}

## To be copied in the UI
# mod_2_2_subctlr_ui("2_2_subctlr_ui_1")

## To be copied in the server
# mod_2_2_subctlr_server("2_2_subctlr_ui_1", r)
