#' 4_1_script UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_1_script_ui <- function(id){
  ns <- NS(id)
  tagList(
    HTML("<b> Create a batch script to index your files </b> <br>"),
    shinyWidgets::pickerInput(inputId = ns("timefield"),label = "1. timefield", choices = c("Creation", "Last Accessed", "Last Written"),
                              selected = c("Creation", "Last Written"), multiple = TRUE),
    shiny::textInput(inputId = ns("drives"),label = "2. path (or drive letter)",value = "H|N"),
    shiny::textInput(inputId = ns("delimiter"),label = "2.path delimiter",value = "|"),
    shinyWidgets::downloadBttn(outputId = ns("createScript"), label = "3. Export Script")
  )
}
    
#' 4_1_script Server Functions
#'
#' @noRd 
mod_4_1_script_server <- function(input, output, session, r){
    ns <- session$ns
 
    shiny::observeEvent(input$timefield,{
      print("RUN timefield")
      print(input$timefield)
      r$timefield <- input$timefield
      r$WCA <- c("C","A","W")[c("Creation", "Last Accessed", "Last Written") %in% r$timefield]
    },ignoreInit = FALSE)
    
    shiny::observeEvent(c(input$drives,input$delimiter),{
      print("RUN drives")
      r$delimiter <- input$delimiter
      r$drives <- unlist(lapply(strsplit(input$drives,split = r$delimiter, fixed=TRUE),trimws))
    },ignoreInit = FALSE)
    
    
    output$createScript <- shiny::downloadHandler(
      filename = function() {
        paste0("index_",paste0(r$WCA,collapse=""),gsub("-","",Sys.Date()),".bat") # "index_20220701.bat"
      },
      content = function(fName) {
        print("RUN createScript")
        wca <- c("C","W","A")[c("C","W","A") %in% r$WCA]
        if(is.null(wca)) wca <- "W"
        driveList <- expand.grid("wca" = wca,"drives" = r$drives)
        
        headerText <- '@echo off
For /f "tokens=1-6 delims=/ " %%a in (\'date /t\') do (set mydate=%%c%%a%%b)
For /f "tokens=1-2 delims=/:" %%a in (\'time /t\') do (set mytime=%%a%%b)
echo %mydate% with created written and or access dates'
        repText_1_1 <-  ' echo "SHARE DRIVE ('
        repText_1_2 <- ':) by '
        repText_1_3 <- '"'
        repText_2_0 <- 'DIR '
        repText_2_0_1 <- '"'
        repText_2_1 <- ':' # only valid for single letters
        # repText_2_1_1 <- "\\"
        repText_2_1_2 <- '"'
        repText_2_2 <- ' /A /T:'
        repText_2_2_1 <- ' /S /Q /R /N /-C > '
        repText_2_3 <- 'v'
        repText_2_4 <- '_%mydate%.txt'
        footerText <- 'msg /time:2.5 %username% "finished query" '
        
        addDrive <- function(X,Y) c(paste0(repText_1_1,X,repText_1_2,
                                           ifelse("C" %in% Y,"Creation",
                                                  ifelse("W" %in% Y,"Last Written",
                                                         "Last Accessed"))," Date",repText_1_3,collapse=""),
                                    paste0(repText_2_0,repText_2_0_1,X,
                                           ifelse(nchar(as.character(X))==1,repText_2_1,""),
                                           ifelse(! substring(X,nchar(as.character(X))) %in% c("\\","/"),ifelse(grepl("/",X),"/","\\"),""),
                                           repText_2_1_2,
                                           repText_2_2,
                                           ifelse("C" %in% Y,"C",
                                                  ifelse("W" %in% Y,"W",
                                                         "A")),repText_2_2_1,
                                           substring(gsub("\\\\","",X),1,1),repText_2_3,
                                           Y,repText_2_4,
                                           collapse="") )
        
        base::write(c(headerText,
                      sapply(1:dim(driveList)[1],function(i) {
                        addDrive(X = driveList$drives[i], Y = driveList$wca[i])
                      }),
                      footerText), file = fName)
      },contentType = "text/csv")
    
    
    
  }
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
