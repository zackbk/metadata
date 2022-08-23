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
    HTML("<b> 1. Create a batch script to index files. </b> <br>"),
    shiny::selectInput(inputId = ns("timefield"),label = "i. timefield", choices = c("Creation", "Last Accessed", "Last Written"), selected = c("Creation", "Last Written"), multiple = TRUE),
    shiny::textInput(inputId = ns("drives"),label = "ii. directory (e.g. drive)",value = "H|N"),
    shiny::textInput(inputId = ns("delimiter"),label = "ii. delimiter",value = "|"),
    #HTML("Note: You can re-run scripts periodically using a task scheduler <br>"),
    shinyWidgets::downloadBttn(outputId = ns("createScript"), label = "iii. Export Script"),
    HTML("<br> <b> 2. Convert into table (.txt or .Rdata) </b> <br>"),
    shiny::fileInput(inputId = ns("uploadIndex"),label = "iv/vii. Upload Files (batch output, or app-csv) ",multiple = TRUE), # also processes the file
    shiny::plotOutput(outputId = "runtime",height = 1),
    shiny::checkboxInput(inputId = ns('compareData'),label = "unify output",value = TRUE),
    #shiny::checkboxInput(inputID = ns('unchangedEntries'),label = "show unchanged files",value = TRUE),
    shinyWidgets::actionBttn(inputId = ns("clear"),label = "delete",icon = icon("trash")),
    shinyWidgets::downloadBttn(outputId = ns("downloadData"),label = "vi. All", size = "md", block = FALSE), # allows the user to download the final processed info (in .RData for starters)
    shinyWidgets::downloadBttn(outputId = ns("export"),label = "v. Filtered", style = "material-flat", color = "default", size = "md", block = FALSE)
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
  
  shiny::observeEvent(input$timefield,{
    print("RUN timefield")
    r$timefield <- input$timefield
    r$WCA <- c("C","A","W")[c("Creation", "Last Accessed", "Last Written") %in% r$timefield]
  },ignoreInit = FALSE)
  
  shiny::observeEvent(c(input$drives,input$delimiter),{
    print("RUN drives")
    r$delimiter <- input$delimiter
    r$drives <- unlist(lapply(strsplit(input$drives,split = r$delimiter, fixed=TRUE),trimws))
  },ignoreInit = FALSE)
  
  observeEvent(input$uploadIndex$datapath, {
    print("RUN uploadIndex")
    shinymaterial::material_spinner_show(session = session, output_id = "runtime")
    
    DT <- data.table::data.table()
    
    for(i in 1:length(input$uploadIndex$datapath) ) {
      print(i)
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
          print(e)
        })
      } else if (sum(csv)>0) {
        DT_temp <- data.table::fread(input$uploadIndex$datapath[i],colClasses = colClass)
      }
      if(sum(rda,csv,txt)>0) {
        
        if(i == 1 & (dim(DT)[1]==0 | input$compareData==FALSE)) { # if no data exists or comparison is off
          DT <- data.table::rbindlist(l = list(DT,DT_temp),use.names = TRUE,fill = TRUE)
          DT[, `:=` ('chng_type' = "", 'chng_sum' = "")]
        } else{
          onCols <- names(DT_temp)[names(DT_temp) %in% names(DT)] # must be before idxPath & idxFile
          doNotCompare <- c("chng_type","chng_sum", "DateCreatedIndexDate", "DateWrittenIndexDate", "DateAccessedIndexDate")
          onCols <- onCols[!onCols %in% doNotCompare]
          
          if((input$compareData == TRUE)){ # data "comparison version"
            # the logic for deciding which one is old versus new is irrelevant at this point.
            # compare original table and table being added
            common_DNC <- doNotCompare[doNotCompare %in% names(DT) & doNotCompare %in% names(DT_temp)] # remove 
            rbL <- function() {
              ansIssue = TRUE
              tryCatch(expr = {
                DT_ans <- data.table::as.data.table(
                  compareDF::compare_df(df_new = DT_temp[,..onCols],df_old = DT[,..onCols],keep_unchanged_rows = TRUE,group_col = onCols)$comparison_df
                )
                ansIssue = FALSE
              }, error = function(e) {
                warning("comparedf issue - 2222222222222")
                print(e)
              })
              
              if(ansIssue == TRUE) { # common data is the same.
                print("ansIssue")
                DT_ans <- DT
                DT_ans[is.na(chng_type), chng_type := "="]
                if(is.null(DT_ans$chng_sum)) DT_ans[, chng_sum := ""]
              }
              
              DT_fin <- DT_ans[!duplicated(DT_ans),]
              DT_DNC <- DT_temp[,!..common_DNC] # if common "old" and "new" cols exist, keep only old DNC cols in the inner join for consistency. 
              print("DT_DNC")
              print(i)
              print(names(DT_DNC))
              
              # note the order of file reading is currently arbitrary
              
              #DT_eq <- DT_temp[DT_DNC[DT_fin[chng_type ==  "=",],on=c(onCols)],on=c(onCols)]
              DT_eq <- DT_DNC[DT_fin[chng_type == "=",],on=c(onCols), nomatch=NULL]
              if(dim(DT_eq)[1]>0) DT_eq[, `:=`(chng_type = "=",
                                               chng_sum = paste0(chng_sum,"=",i))]
              if(ansIssue == TRUE){
                return ( list(DT_eq) )
              } else{
                DT_su <- DT[DT_fin[chng_type ==  "-",],on=c(onCols)]
                if(dim(DT_su)[1]>0) DT_su[, `:=`(chng_type = "-",
                                                 chng_sum = paste0(chng_sum,"-",i))]
                
                DT_ad <- DT_temp[DT_fin[chng_type ==  "+",],on=c(onCols)]
                if(dim(DT_ad)[1]>0) DT_ad[, `:=`(chng_type = "+",
                                                 chng_sum = paste0(chng_sum,"+",i))]
                list(DT_eq, # unchanged
                     DT_su, # old
                     DT_ad # new
                )
              }
            }
          } else{
            rbL <- function() list(DT_temp[DT,on=c(onCols)],DT_temp[!DT,on=c(onCols)])
          }
          DT <- data.table::rbindlist(l = rbL(),
                                      use.names = TRUE,
                                      fill = TRUE)
        }
      }
    }
    print("cleaning data")
    if("DateAccessed" %in% names(DT)) DT[, DateAccessed := toDateTime(DT$DateAccessed)]
    if("DateWritten" %in% names(DT)) DT[, DateWritten := toDateTime(DT$DateWritten)]
    if('DateCreated' %in% names(DT)) DT[, DateCreated := toDateTime(DT$DateCreated)]
    if("Owner" %in% names(DT)) DT[, Owner := gsub("\\\\","/",Owner)]
    DT <- addIcon(DT)
    data.table::setcolorder(DT, neworder = colOrder[colOrder %in% names(DT)])
    r$O2 <- DT
    
    r$T2 <- r$O2[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = ext, .SDcols = c("TotalByteSize", "TotalFileCount")][
      order(-TotalByteSize*TotalFileCount),
      ]
    
    r$T3 <- r$O2[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = Owner, .SDcols = c("TotalByteSize", "TotalFileCount")][
      order(-TotalByteSize*TotalFileCount),
      ]
    shinymaterial::material_spinner_show(session = session, output_id = "runtime")
    print("finished UPLOAD")
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
  
  
  
  output$downloadData <- shiny::downloadHandler(
    filename = function() {
      paste0("SDA-",Sys.Date(),".csv",sep="")
    },
    content = function(fName) {
      print("Download Data")
      data.table::fwrite(x = r$temp, file = fName)
    }
  )
  
  output$export <- shiny::downloadHandler(
    filename = function() {
      paste0("searchResults",Sys.Date(),".html")
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
  print("End mod_4")
}

## To be copied in the UI
# 

## To be copied in the server
# 
