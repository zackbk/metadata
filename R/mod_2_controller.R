#' 2_controller UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_controller_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::sliderInput(inputId = ns("dateFrom"), label = "From" ,
                       min = min(O2Clean$DateWritten),
                       max = max(O2Clean$DateWritten),
                       value = min(O2Clean$DateWritten)),
    shiny::sliderInput(inputId = ns("dateTo"), label = "To" ,
                       min = min(O2Clean$DateWritten),
                       max = max(O2Clean$DateWritten),
                       value = max(O2Clean$DateWritten)),
    shiny::selectInput(inputId = ns("fileOrFolder"),label = "Type",choices = unique(O2Clean$Type), selected = unique(O2Clean$Type)[1],multiple = TRUE),
    shinyWidgets::searchInput(inputId = ns("searchString"), label = "Search", placeholder = "File Name",btnSearch = icon("search",lib = "font-awesome")),
    shinyWidgets::searchInput(inputId = ns("parentString"), label = "", placeholder = "Folder (if known)",btnSearch = icon("search-plus",lib = "font-awesome")),
    shinyWidgets::switchInput(inputId = ns("ignoreCase"), label = "ignore_case", value = TRUE),
    shiny::selectizeInput(inputId = ns("extensionName"), label = "extension (if known)",
                          choices = T2$Extension,
                          multiple = TRUE),
    shinyWidgets::noUiSliderInput(inputId = ns("folderDepth"), 
                                  label = "minimum folder depth", 
                                  min = min(O2Clean$Level),
                                  max = max(O2Clean$Level),
                                  value = min(O2Clean$Level), step = 1),
    shiny::selectInput(inputId = ns("arrangeBy"),label = "Arrange by",choices = c("",names(O2Clean)), multiple = FALSE),
    shinyWidgets::switchInput(inputId = ns('order'), onLabel = "Z-A",offLabel = "A-Z",value = FALSE),
    shiny::actionButton(inputId = ns("resetSearch"), label = "Reset"),
    shiny::actionButton(inputId = ns("subsetSearch"),label = "Subset"),
    shiny::actionButton(inputId = ns("copyFiles"), label = "Copy Files"),
    shiny::textInput(inputId = ns("copyTo"),label = "Copy To",value = paste0(getwd(),"/COPY"))
  )
}
    
#' 2_controller Server Function
#'
#' @noRd 
mod_2_controller_server <- function(input, output, session, r){
  ns <- session$ns
  
  observeEvent(c(input$fileOrFolder,input$folderDepth, input$dateFrom, input$dateTo, input$extensionName, input$searchString, input$ignoreCase, input$parentString),
               {
                 print("search")
                 print(dim(r$temp))
                 r$temp <- SearchModel(x = r$temp, input)
               },ignoreInit = FALSE, priority = 0)
  
  
  observeEvent(input$resetSearch, {
    r$temp <- data.table::copy(O2Clean)
    r$temp[, filter := TRUE]
  }, priority = 5)
  
  observeEvent(input$subsetSearch, {
    print("subset")
    print(dim(r$temp))
    r$temp <- SearchModel(x = r$temp, input)[filter == TRUE, ]
    #r$temp <- subset
    print("post-subset")
    print(dim(r$temp))
  }, priority = 3)
  
  observeEvent(input$copyFiles,{
    #lapply(r$temp$pathString[r$temp$filter == TRUE], function(x) shell(paste0('echo F| XCOPY "',x,'" "',getwd(),'/XCOPY"') ))
    updateActionButton(inputId = "copyFiles", session = session, label =  "Copying ...")
    if(dir.exists(input$copyTo) == FALSE) input$copyTo
    
    
    myFiles <- paste0(r$temp[r$temp$filter == TRUE & Type == "File"]$pathString,
                      ifelse(r$temp[r$temp$filter == TRUE & Type == "File"]$Extension=="","","."),
                      r$temp[r$temp$filter == TRUE & Type == "File"]$Extension)
    print("")
    lapply(myFiles, function(x) file.copy(from = x,to = input$copyTo ) )
    
    
    updateActionButton(inputId = "copyFiles", session = session, label =  "Done!")
    Sys.sleep(2)
    updateActionButton(inputId = "copyFiles", session = session, "Copy Files")
  }, priority = -1)
  
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
 
