#' @param x 
#'   the data table to be filtered which should have the following values:
#'   `DateWritten`.
#' @param input
#'   includes all the various controlllers for filtering the data table (x).
#' @return x filtered and modified version of x which includes
#'   `filter` this is a boolean column which highlights the filtered values.

# return x filtered, searched, and arranged based on input parameters (file-or-folder, date, search string, Level, Extension, parent search string)
SearchModel <- function(x,input) {
  print("SearchModel")
  x <- data.table::copy(x)
  x[, filter := TRUE]
  if("fileOrFolder" %in% names(x) & length(input$fileOrFolder)>0)
    x[ (filter == TRUE) & 
         !(Type %in% input$fileOrFolder),
       filter := FALSE ]
  if("DateCreated" %in% names(x) & length(input$createdFrom)>0 & length(input$createdTo)>0)
    x[ (filter == TRUE) & 
         !(DateCreated >= input$createdFrom & DateCreated <= input$createdTo),
       filter := FALSE]
  if("DateWritten" %in% names(x) & length(input$writtenFrom)>0 & length(input$writtenTo)>0)
    x[ (filter == TRUE) & 
         !(DateWritten >= input$writtenFrom & DateWritten <= input$writtenTo),
       filter := FALSE]
  if("DateAccessed" %in% names(x) & length(input$accessedFrom)>0 & length(input$accessedTo)>0)
    x[ (filter == TRUE) & 
         !(DateAccessed >= input$accessedFrom & DateAccessed <= input$accessedTo),
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
  if ("Level" %in% names(x) & length(input$minFolderDepth) > 0) {
    x[(filter == TRUE) & 
        !(Level >= input$minFolderDepth),
      filter := FALSE ]
  }
  if ("Owner" %in% names(x) & length(input$Owner) > 0 ) {
    x[(filter == TRUE) & 
        !(tolower(Owner) %in% tolower(input$Owner)),
      filter := FALSE]
  }
  if ("Extension" %in% names(x) & length(input$extensionName) > 0) {
    x[(filter == TRUE) & 
        !(tolower(Extension) %in% tolower(input$extensionName) ), # extensions are always searched in lower case.
      filter := FALSE]
  }
  if (length(input$parentString) > 0) {
    if("parentName" %in% names(x) &
       length(input$parentString)>0 & 
       input$parentString != "" & 
       length(input$ignoreCase)>0) {
      x[(filter == TRUE) & 
          !stringr::str_detect(string = parentName, 
                               pattern = stringr::regex(
                                 pattern = input$parentString, 
                                 ignore_case = input$ignoreCase)),
        filter := FALSE]
      
    }
  }
  return(x)
}