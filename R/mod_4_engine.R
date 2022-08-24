#' 4_engine UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_4_engine_ui <- function(id){
  ns <- NS(id)
  tagList(
    #HTML("<br> <b> 2. Produce table (from .txt result) </b> <br>"),
    shiny::fileInput(inputId = ns("uploadIndex"),label = "Upload",multiple = TRUE), # also processes the file
    # shiny::plotOutput(outputId = "runtime",height = 1),
    # shinyWidgets::progressBar(id = ns("runtime"), value = 0, total = 100, title = "", display_pct = TRUE),
    shinyWidgets::actionBttn(inputId = ns("exampleData"),label = "mtcars",icon = icon("car"),color = 'success',size = 'sm', style = 'material-flat'),
    shinyWidgets::actionBttn(inputId = ns("clear"),label = "clear",icon = icon("trash"),color = 'danger',size = 'sm', style = 'material-flat'),
    shinyWidgets::downloadBttn(outputId = ns("downloadData"),label = "save csv", size = "sm",color = 'primary', style = 'material-flat') # allows the user to download the final processed info (in .RData for starters)
  )
}

#' 4_engine Server Functions
#'
#' @noRd 
mod_4_engine_server <- function(input, output, session, r){
  ns <- session$ns
  print("RUN mod_4")
  
  shiny::observeEvent(input$clear,{
    r$O2 <- O2Empty
  },ignoreInit = TRUE)
  
  observeEvent( input$exampleData, {
    print("RUN sample")
    DT <- data.table::data.table(mtcars)
    DT[,model := gsub("^[A-z0-9]* ","",rownames(mtcars))]
    DT[,make := gsub(" [A-z0-9]*","",rownames(mtcars))]
    data.table::setcolorder(DT,c("make","model"))
    r$O2 <- DT
    
  },ignoreInit = FALSE,priority = 100)
  
  observeEvent(input$uploadIndex$datapath, {
    print("RUN uploadIndex")
    shinyWidgets::progressSweetAlert(session= session,id = ns("runtime"),title = "WIP",display_pct = TRUE, value = 10)
    
    DT <- data.table::data.table()
    
    iMax <- length(input$uploadIndex$datapath)
    for(i in 1:length(input$uploadIndex$datapath) ) {
      shinyWidgets::updateProgressBar(session = session,id = ns("runtime"), value = 10 + 80*((i-1)/iMax)) 
      rm(DT_temp)
      txt <- grepl(pattern = "\\.txt$",x = input$uploadIndex$datapath[i],ignore.case = T) # text file is raw file index
      rda <- grepl(pattern = "\\.rda$",x = input$uploadIndex$datapath[i],ignore.case = T)
      csv <-  grepl(pattern = "\\.csv$",x = input$uploadIndex$datapath[i],ignore.case = T)
      
      if(sum(rda)>0) {
        load(input$uploadIndex$datapath[i],envir = .GlobalEnv)
        #DT_temp <- base::get() # under development
      } else if(sum(txt)>0) { # 1.0 
        tryCatch(expr = {
          DT_temp <- GetO2(myDir = input$uploadIndex$datapath[i],FName = input$uploadIndex$name[i])
        }, error = function(e) {
          warning("GetO2 error")
          DT_temp <- O2Empty
          print(e)
        })
      } else if (sum(csv)>0) {
        DT_temp <- data.table::fread(input$uploadIndex$datapath[i],colClasses = colClass)
        
      }
      shinyWidgets::updateProgressBar(session = session,id = ns("runtime"), value = 10 + 40*(1/iMax) + 80*((i-1)/iMax)) 
      if(sum(rda,csv,txt)>0) {
        
        if(i == 1 | csv > 0) { # if no data exists or comparison is off
          DT <- data.table::rbindlist(l = list(DT,DT_temp),use.names = TRUE,fill = TRUE)
        } else{
          onCols <- names(DT_temp)[names(DT_temp) %in% names(DT)] # must be before idxPath & idxFile
          doNotCompare <- c("chng_type","chng_sum", "DateCreatedIndexDate", "DateWrittenIndexDate", "DateAccessedIndexDate")
          onCols <- onCols[!onCols %in% doNotCompare]
          
          rbL <- function() list(DT_temp[DT,on=c(onCols)],DT_temp[!DT,on=c(onCols)])
          DT <- data.table::rbindlist(l = rbL(),
                                      use.names = TRUE,
                                      fill = TRUE)
        }
      }
    }
    
    shinyWidgets::updateProgressBar(session = session,id = ns("runtime"), value = 90) 
    
    print("cleaning data")
    if("DateAccessed" %in% names(DT)) DT[, DateAccessed := toDateTime(DT$DateAccessed)]
    if("DateWritten" %in% names(DT)) DT[, DateWritten := toDateTime(DT$DateWritten)]
    if('DateCreated' %in% names(DT)) DT[, DateCreated := toDateTime(DT$DateCreated)]
    if("Owner" %in% names(DT)) DT[, Owner := gsub("\\\\","/",Owner)]
    DT <- addIcon(DT)
    data.table::setcolorder(DT, neworder = colOrder[colOrder %in% names(DT)])
    r$O2 <- DT

    if(sum(!c('Type','ext',"TotalByteSize", "TotalFileCount",'Owner') %in% names(r$O2)) == 0){
      
    r$T2 <- r$O2[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = ext, .SDcols = c("TotalByteSize", "TotalFileCount")][
      order(-TotalByteSize*TotalFileCount),
      ]
    
    r$T3 <- r$O2[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = Owner, .SDcols = c("TotalByteSize", "TotalFileCount")][
      order(-TotalByteSize*TotalFileCount),
      ]
    }
    
    shinyWidgets::closeSweetAlert(session = session)
    shinyWidgets::sendSweetAlert(session = session, title =" Processing complete!",type = "success")
    print("finished UPLOAD")
  },ignoreInit = FALSE)
  
  
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste0("SDA-",Sys.Date(),".csv",sep="")
    },
    content = function(fName) {
      print("Download Data")
      data.table::fwrite(x = r$temp, file = fName)
    }
  )
  

  print("End mod_4")
}

## To be copied in the UI
# 

## To be copied in the server
# 
