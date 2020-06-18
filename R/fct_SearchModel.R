#' @param x 
#'   the data table to be filtered which should have the following values:
#'   `DateWritten`.
#' @param input
#'   includes all the various controlllers for filtering the data table (x).
#' @return x filtered and modified version of x which includes
#'   `filter` this is a boolean column which highlights the filtered values.

# return x filtered, searched, and arranged based on input parameters (file-or-folder, date, search string, Level, Extension, parent search string)
SearchModel <- function(x,input) {
  
  x <- data.table::copy(x)
  x[, filter := TRUE]
  x[ (filter == TRUE) & 
       !(Type %in% input$fileOrFolder),
     filter := FALSE ]
  x[ (filter == TRUE) & 
       !(DateWritten >= input$dateFrom & DateWritten <= input$dateTo),
     filter := FALSE]
  if (length(input$searchString) > 0) {
    if(input$searchString != "") {
      x[(filter == TRUE) & 
          !stringr::str_detect(string = Name, 
                               pattern = stringr::regex(
                                 pattern = input$searchString, 
                                 ignore_case = input$ignoreCase)),
        filter := FALSE]
    }
  }
  if (length(input$folderDepth) > 0) {
    x[(filter == TRUE) & 
        !(Level >= input$folderDepth),
      filter := FALSE ]
  }
  if (length(input$extensionName) > 0) {
    x[(filter == TRUE) & 
        !(Extension %in% input$extensionName),
      filter := FALSE]
  }
  if (length(input$parentString) > 0) {
    if(input$parentString != "") {
      x[(filter == TRUE) & 
          !stringr::str_detect(string = parentName, 
                               pattern = stringr::regex(
                                 pattern = input$parentString, 
                                 ignore_case = input$ignoreCase)),
        filter := FALSE]
      
    }
  }
  if(input$arrangeBy != "") {
    return( x[order(x[[input$arrangeBy]],decreasing = input$order)] )
  } else {
    return(x)  
  }
}