#' 2_search UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_2_search_ui <- function(id){
  ns <- NS(id)
  tagList(
    # search bar : 
    # 
    shiny::fluidRow(
      shinyWidgets::searchInput(inputId = ns("searchString"), #label = "Search",
                                placeholder = "File Name",width = "100%"),
      # advanced search
      shinyWidgets::searchInput(inputId = ns("parentString"), label = "", placeholder = "Folder",btnSearch = icon("search-plus",lib = "font-awesome")),
      # shinyWidgets::actionBttn(inputId = ns("searchString_search"),icon = icon("search"),style = "material-flat",size = "xs"),
      shinyWidgets::actionBttn(inputId = ns("searchString_reset"),label = 'clear',icon = icon("remove"),style = "material-flat",size = "xs")
    ),
    # shiny::selectInput(inputId = ns("fileOrFolder"),label = "Type",
    #                    choices = unique(O2Empty$Type)[order(unique(O2Empty$Type))],
    #                    selected = unique(O2Empty$Type),multiple = TRUE),
    # shinyWidgets::noUiSliderInput(inputId = ns("minFolderDepth"),
    #                               label = "min folder depth",
    #                               min = 1,
    #                               max = 25,
    #                               value = 1,step = 1),
    # shinyWidgets::noUiSliderInput(inputId = ns("maxFolderDepth"),
    #                               label = "max folder depth",
    #                               min = 1,
    #                               max = 25,
    #                               value = 25,step = 1),
    # filter :
    # shiny::checkboxInput(inputId = ns("ignoreCase"), label = "ignore_case", value = TRUE),
    shinyWidgets::pickerInput(inputId = ns("Owner"), label = "Owner",
                              choices = T3Empty$Owner,
                              inline = TRUE,
                              multiple = TRUE),
    shinyWidgets::pickerInput(inputId = ns("extensionName"), label = "extension (if it's a file)",
                              choices = T2Empty$ext,
                              inline = TRUE,
                              multiple = TRUE)
  )
}

#' 2_search Server Function
#'
#' @noRd 
mod_2_search_server <- function(input, output, session, r){
  ns <- session$ns
  print("RUN mod_2")
  # Change to CORE DATA
  # observeEvent(c(dim(r$O2()),input$resetSearch),{ # run after upload
  #   print("run r$temp")
  #   if("DateWritten" %in% names(r$temp)){
  #     print("date written")
  #     shiny::updateSliderInput(session = session, inputId = "writtenFrom",
  #                              min = min(r$temp$DateWritten,na.rm=T),
  #                              max = as.POSIXct(Sys.time()),
  #                              value = min(r$temp$DateWritten,na.rm=T))
  #     shiny::updateSliderInput(session = session, inputId = "writtenTo",
  #                              min = min(r$temp$DateWritten,na.rm=T),
  #                              max = as.POSIXct(Sys.time()),
  #                              value = max(r$temp$DateWritten,na.rm=T))
  #   }
  #   if("DateCreated" %in% names(r$temp)){
  #     print("date created")
  #     shiny::updateSliderInput(session = session, inputId = "createdFrom",
  #                              min = min(r$temp$DateCreated,na.rm=T),
  #                              max = as.POSIXct(Sys.time()),
  #                              value = min(r$temp$DateCreated,na.rm=T))
  #     shiny::updateSliderInput(session = session, inputId = "createdTo",
  #                              min = min(r$temp$DateCreated,na.rm=T),
  #                              max = as.POSIXct(Sys.time()),
  #                              value = max(r$temp$DateCreated,na.rm=T))
  #   }
  #   if("DateAccessed" %in% names(r$temp)){
  #     print("date accessed")
  #     shiny::updateSliderInput(session = session, inputId = "accessedFrom",
  #                              min = min(r$temp$DateAccessed,na.rm=T),
  #                              max = as.POSIXct(Sys.time()),
  #                              value = min(r$temp$DateAccessed,na.rm=T))
  #     shiny::updateSliderInput(session = session, inputId = "accessedTo",
  #                              min = min(r$temp$DateAccessed,na.rm=T),
  #                              max = as.POSIXct(Sys.time()),
  #                              value = max(r$temp$DateAccessed,na.rm=T))
  #   }
  #   if(length(r$temp$Level)>0){
  #     print("level")
  #     shinyWidgets::updateNoUiSliderInput(session = session,inputId = "minFolderDepth", 
  #                                         range = c(min(r$temp$Level,na.rm=T),max(r$temp$Level,na.rm=T)),
  #                                         value = min(r$temp$Level,na.rm=T))
  #     shinyWidgets::updateNoUiSliderInput(session = session,inputId = "maxFolderDepth", 
  #                                         range = c(min(r$temp$Level,na.rm=T),max(r$temp$Level,na.rm=T)),
  #                                         value = max(r$temp$Level,na.rm=T))
  #   }
  #   
  # },ignoreInit = TRUE)
  
  #
  observeEvent(c(dim(r$T2)),{ # run after upload
    # probably a better way to do than DIM, but for now this is ok.
    print('run dim(r$T2)')
    shinyWidgets::updatePickerInput(session = session, inputId = "extensionName",
                                    choices = r$T2$ext)
  },ignoreInit = FALSE)
  observeEvent(c(dim(r$T3)),{
    print('run dim(r$T3)')
    shinyWidgets::updatePickerInput(session = session, inputId = "Owner",
                                    choices = r$T3$Owner)
  },ignoreInit = FALSE)
  # SEARCH
  observeEvent(c(input$searchString, 
                 # input$ignoreCase, 
                 input$parentString,
                 input$extensionName,
                 input$Owner
                 # input$searchString_search
  ), {
    print("search")
    print(dim(r$temp))
    r$temp <- SearchModel(x = r$temp, input)
    for(i in c("searchString","parentString",
               # "ignoreCase",
               "Owner",
               # "writtenFrom","writtenTo"
               # ,"accessedFrom","accessedTo",
               # "createdFrom","createdTo",
               "extensionName"
    ) ){
      r$searchOptions[[paste0(i)]] <- unlist(input[[i]])
    }
  },ignoreInit = TRUE, priority = 0)
  
  
  observeEvent(c(input$searchString_reset), {
    print("reset")
    r$temp <- data.table::copy(r$O2())
    r$temp[, filter := TRUE]
  }, priority = 5)
  
  print("End mod_2")
}

## To be copied in the UI
# 

## To be copied in the server
# 

