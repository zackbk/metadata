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
    #shinyWidgets::actionBttn(inputId = ns("exampleData"),icon = icon("car"),color = 'success',size = 'sm', style = 'material-flat',block=FALSE),
    shinyWidgets::pickerInput(inputId = ns("pickData"),choices = c("auto","mtcars"),selected = "auto",multiple = FALSE),
    shinyWidgets::downloadBttn(outputId = ns("downloadData"),label = "DL", size = "sm",color = 'primary', style = 'material-flat'), # allows the user to download the final processed info (in .RData for starters)
    shinyWidgets::actionBttn(inputId = ns("clear"),icon = icon("trash"),color = 'danger',size = 'sm', style = 'material-flat',block=FALSE)
    
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
  
  
  observeEvent( input$pickData, {
    print("RUN sample")
    if(input$pickData %in% "auto"){
      # wait for data to be uploaded
    } else if(input$pickData %in% names(sampleDT)) {
      r$O2 <- sampleDT[[input$pickData]]
    }
    
  },ignoreInit = FALSE,priority = 100)
  
  observeEvent(input$uploadIndex$datapath, {
    print("RUN uploadIndex")
    shinyWidgets::progressSweetAlert(session= session,id = ns("runtime"),title = "WIP",display_pct = TRUE, value = 10)
    j <- length(sampleDT) + 1
    DT <- data.table::data.table()
    
    iMax <- length(input$uploadIndex$datapath)
    for(i in 1:length(input$uploadIndex$datapath) ) {
      shinyWidgets::updateProgressBar(session = session,id = ns("runtime"), value = 10 + 80*((i-1)/iMax)) 
      rm(DT_temp)
      txt <- grepl(pattern = "\\.txt$",x = input$uploadIndex$datapath[i],ignore.case = T) # text file is raw file index
      rda <- grepl(pattern = "\\.rda$",x = input$uploadIndex$datapath[i],ignore.case = T)
      csv <-  grepl(pattern = "\\.csv$",x = input$uploadIndex$datapath[i],ignore.case = T)
      xl <- grepl(pattern = "\\.xls$|\\.xlsx$",x = input$uploadIndex$datapath[i],ignore.case = T)
      
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
      } else if (sum (xl)>0){
        DT_temp <- data.table::as.data.table(readxl::read_excel(input$uploadIndex$datapath[i]))
      }
      shinyWidgets::updateProgressBar(session = session,id = ns("runtime"), value = 10 + 40*(1/iMax) + 80*((i-1)/iMax)) 
      
      names(DT_temp) <- gsub("[,.|\\()]",".",names(DT_temp))
      names(DT_temp) <- gsub("[/\\]","div",names(DT_temp))
      names(DT_temp) <- gsub("[%]","pct",names(DT_temp))
      names(DT_temp) <- gsub("[$]","dol",names(DT_temp))
      names(DT_temp) <- gsub("[&]","and",names(DT_temp))
      names(DT_temp) <- gsub("[!]","exc",names(DT_temp))
      names(DT_temp) <- gsub("[=]","eq",names(DT_temp))
      names(DT_temp) <- gsub("[+]","plus",names(DT_temp))
      names(DT_temp) <- gsub("[-]","dash",names(DT_temp))
      names(DT_temp) <- gsub("[*]","mul",names(DT_temp))
      names(DT_temp) <- gsub("[\\^]","pwr",names(DT_temp))
      names(DT_temp) <- gsub("[>]","gt",names(DT_temp))
      names(DT_temp) <- gsub("[<]","lt",names(DT_temp))
      names(DT_temp) <- gsub("[[:punct:]]","",names(DT_temp))
      names(DT_temp) <- gsub("\\s","",names(DT_temp))
      if(sum(rda,csv,txt,xl)>0) {
        
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
    
    # section to fix dates upon upload
    date_cols <- grep("date",names(DT),ignore.case = TRUE)
    for(dc in date_cols) DT[, (names(DT)[dc]) := toDateTime(DT[[names(DT)[dc]]])]
    
    if("DateAccessed" %in% names(DT)) DT[, DateAccessed := toDateTime(DT$DateAccessed)]
    if("DateWritten" %in% names(DT)) DT[, DateWritten := toDateTime(DT$DateWritten)]
    if('DateCreated' %in% names(DT)) DT[, DateCreated := toDateTime(DT$DateCreated)]
    if("Owner" %in% names(DT)) DT[, Owner := gsub("\\\\","/",Owner)]
    
    DT <- addIcon(DT)
    data.table::setcolorder(DT, neworder = colOrder[colOrder %in% names(DT)])
    
    sampleDT[[j]] <- DT
    names(sampleDT)[j] <- input$uploadIndex$name[i]
    
    shinyWidgets::updatePickerInput(session = session,inputId = 'pickData', selected = 'auto',
                                    choices = names(sampleDT))
    
    r$O2 <- sampleDT[[j]]
      
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
