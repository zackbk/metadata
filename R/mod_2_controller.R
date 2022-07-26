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
    # search bar : 
    # 
    shinymaterial::material_row(
    shinymaterial::material_column(
      width =  6,
      shinyWidgets::searchInput(inputId = ns("searchString"), #label = "Search",
                                placeholder = "Search File Name",width = "100%"),
    ),
      shinymaterial::material_column(
        width = 3,
        shinyWidgets::actionBttn(inputId = ns("searchString_search"),icon = icon("search"),style = "material-flat",size = "lg" )
      ),
      shinymaterial::material_column(
        width = 3,
        shinyWidgets::actionBttn(inputId = ns("searchString_reset"),icon = icon("remove"),style = "material-flat",size = "lg" )
      )
    ),
    # shinymaterial::material_row(
      # search settings
      # shinymaterial::material_column(
        # width = 4,
        shinyWidgets::dropMenu(
          tag = shinyWidgets::actionBttn(inputId = ns("folderOptions"),"Folder(s)", style = "material-flat", color = "default", size = "md", block = FALSE, icon = icon("folder")),
          shinyWidgets::searchInput(inputId = ns("parentString"), label = "", placeholder = "Folder (if known)",btnSearch = icon("search-plus",lib = "font-awesome")),
          shiny::selectInput(inputId = ns("fileOrFolder"),label = "Type",
                             choices = unique(O2Empty$Type)[order(unique(O2Empty$Type))],
                             selected = unique(O2Empty$Type),multiple = TRUE),
          shinyWidgets::noUiSliderInput(inputId = ns("minFolderDepth"),
                                        label = "minimum folder depth",
                                        min = 1,
                                        max = 10,
                                        value = 1,step = 1)
        ),
      # ),
      # shinymaterial::material_column(
        # width = 4,
        # filter :
        shinyWidgets::dropMenu(
          tag = shinyWidgets::actionBttn(inputId = ns("filterBy"),"Filter", style = "material-flat", color = "default", size = "md", block = FALSE, icon = icon("filter")),
          shiny::checkboxInput(inputId = ns("ignoreCase"), label = "ignore_case", value = TRUE),
          shiny::selectizeInput(inputId = ns("Owner"), label = "Owner",
                                choices = T3Empty$Owner,
                                multiple = TRUE),
          shiny::selectizeInput(inputId = ns("extensionName"), label = "extension (if it's a file)",
                                choices = T2Empty$ext,
                                multiple = TRUE),
          shiny::sliderInput(inputId = ns("writtenFrom"), label = "Written From" ,
                             min = timeInit,
                             max = timeInit,
                             value = timeInit),
          shiny::sliderInput(inputId = ns("writtenTo"), label = "Written To" ,
                             min = timeInit,
                             max = timeInit,
                             value = timeInit),
          shiny::sliderInput(inputId = ns("createdFrom"), label = "Created From" ,
                             min = timeInit,
                             max = timeInit,
                             value = timeInit),
          shiny::sliderInput(inputId = ns("createdTo"), label = "Created To" ,
                             min = timeInit,
                             max = timeInit,
                             value = timeInit),
          shiny::sliderInput(inputId = ns("accessedFrom"), label = "Accessed From" ,
                             min = timeInit,
                             max = timeInit,
                             value = timeInit),
          shiny::sliderInput(inputId = ns("accessedTo"), label = "Accessed To" ,
                             min = timeInit,
                             max = timeInit,
                             value = timeInit)
        ),
      # ),
      # shinymaterial::material_column(
        # width = 4,
        # actions
        shinyWidgets::dropMenu(
          tag = shinyWidgets::actionBttn(inputId = ns("actions"),"Actions", style = "material-flat", color = "default", size = "md", block = FALSE, icon = icon("gears")),
          shinyWidgets::actionBttn(inputId = ns("copyFiles"), label = "Copy files to Temp Dir", style = "material-flat", size = "md", block = TRUE),
          shiny::textInput(inputId = ns("copyTo"),label = "Temp dir:",value = paste0(tempdir(),"/COPIED")),
          shinyWidgets::actionBttn(inputId = ns("subsetSearch"),label = "Subset search to current results", style = "material-flat", size = "md",block = TRUE),
          shinyWidgets::actionBttn(inputId = ns("resetSearch"), label = "Reset Search [X]", style = "material-flat", size = "md", block = TRUE)
        )
      # )
    # )
  )
}

#' 2_controller Server Function
#'
#' @noRd 
mod_2_controller_server <- function(input, output, session, r){
  ns <- session$ns
  print("RUN mod_2")
  # Change to CORE DATA
  observeEvent(dim(r$temp),{ # run after upload
    print("run r$temp")
    if("DateWritten" %in% names(r$temp)){
      print("date written")
      shiny::updateSliderInput(session = session, inputId = ns("writtenFrom"),
                               min = min(r$temp$DateWritten,na.rm=T),
                               max = as.POSIXct(Sys.time()),
                               value = min(r$temp$DateWritten,na.rm=T))
      shiny::updateSliderInput(session = session, inputId = ns("writtenTo"),
                               min = min(r$temp$DateWritten,na.rm=T),
                               max = as.POSIXct(Sys.time()),
                               value = min(r$temp$DateWritten,na.rm=T))
    }
    if("DateCreated" %in% names(r$temp)){
      print("date created")
      shiny::updateSliderInput(session = session, inputId = ns("createdFrom"),
                               min = min(r$temp$DateCreated,na.rm=T),
                               max = as.POSIXct(Sys.time()),
                               value = min(r$temp$DateCreated,na.rm=T))
      shiny::updateSliderInput(session = session, inputId = ns("createdTo"),
                               min = min(r$temp$DateCreated,na.rm=T),
                               max = as.POSIXct(Sys.time()),
                               value = min(r$temp$DateCreated,na.rm=T))
    }
    if("DateAccessed" %in% names(r$temp)){
      print("date accessed")
      shiny::updateSliderInput(session = session, inputId = ns("accessedFrom"),
                               min = min(r$temp$DateAccessed,na.rm=T),
                               max = as.POSIXct(Sys.time()),
                               value = min(r$temp$DateAccessed,na.rm=T))
      shiny::updateSliderInput(session = session, inputId = ns("accessedTo"),
                               min = min(r$temp$DateAccessed,na.rm=T),
                               max = as.POSIXct(Sys.time()),
                               value = min(r$temp$DateAccessed,na.rm=T))
    }
    if(length(r$temp$Level)>0){
      print("level")
      shinyWidgets::updateNoUiSliderInput(session = session,inputId = ns("minFolderDepth"), 
                                          range = c(min(r$temp$Level,na.rm=T),max(r$temp$Level,na.rm=T)),
                                          value = min(r$temp$Level,na.rm=T))
    }
    
  },ignoreInit = TRUE)
  
  #
  observeEvent(c(dim(r$T2)),{ # run after upload
    # probably a better way to do than DIM, but for now this is ok.
    print('run dim(r$T2)')
    shiny::updateSelectizeInput(inputId = ns("extensionName"),
                                choices = r$T2$ext)
  },ignoreInit = TRUE)
  observeEvent(c(dim(r$T3)),{
    print('run dim(r$T3)')
    shiny::updateSelectizeInput(session = session,inputId = ns("Owner"),
                                choices = r$T3$Owner)
  },ignoreInit = TRUE)
  # SEARCH
  observeEvent(c(input$fileOrFolder,input$minFolderDepth, 
                 input$createdFrom, input$createdTo, input$writtenFrom, input$writtenTo, input$accessedFrom, input$accessedTo, 
                 input$Owner, input$extensionName, input$searchString, input$ignoreCase, input$parentString,
                 input$searchString_search
  ), {
    print("search")
    print(dim(r$temp))
    r$temp <- SearchModel(x = r$temp, input)
    for(i in c("searchString","parentString","ignoreCase","Owner","writtenFrom","writtenTo","accessedFrom","accessedTo",
               "createdFrom","createdTo","extensionName","fileOrFolder","minFolderDepth") ){
      r$searchOptions[[paste0(i,input$subsetSearch)]] <- unlist(input[[i]])
    }
  },ignoreInit = TRUE, priority = 0)
  
  
  observeEvent(c(input$resetSearch,input$searchString_reset,dim(r$O2)), {
    print("reset")
    r$temp <- data.table::copy(r$O2)
    r$temp[, filter := TRUE]
  }, priority = 5)
  
  
  # SUBSET
  observeEvent(input$subsetSearch, {
    print("subset")
    print(dim(r$temp))
    r$temp <- SearchModel(x = r$temp, input)[filter == TRUE, ]
    #r$temp <- subset
    print("post-subset")
    print(dim(r$temp))
  }, priority = 3)
  
  # COPY
  observeEvent(input$copyFiles,{
    print("copyFiles")
    #lapply(r$temp$pathString[r$temp$filter == TRUE], function(x) shell(paste0('echo F| XCOPY "',x,'" "',getwd(),'/XCOPY"') ))
    updateActionButton(inputId = ns("copyFiles"), session = session, label =  "Copying ...")
    if(dir.exists(input$copyTo) == FALSE) input$copyTo
    
    
    myFiles <- paste0(r$temp[r$temp$filter == TRUE & Type == "File"]$pathString,
                      ifelse(r$temp[r$temp$filter == TRUE & Type == "File"]$Extension=="","","."),
                      r$temp[r$temp$filter == TRUE & Type == "File"]$Extension)
    lapply(myFiles, function(x) file.copy(from = x,to = input$copyTo ) )
    
    
    shiny::updateActionButton(inputId = ns("copyFiles"), session = session, label =  "Done!")
    Sys.sleep(2)
    shiny::updateActionButton(inputId = ns("copyFiles"), session = session, "Copy Files")
    shinyalert::shinyalert("Copied to a temporary location, link is in your clipboard")
  }, priority = -1)
  
  print("End mod_2")
}

## To be copied in the UI
# 

## To be copied in the server
# 

