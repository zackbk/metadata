# UI
#' initializeFunctions
#' @param x Value to aggregate.
#' @param ... list of options to pass to the quantile function.
nothing <- function(x,...) x
quantile25 <- function(x,...) stats::quantile(x,0.25,...)
quantile50 <- function(x,...) stats::quantile(x,0.50,...)
quantile75 <- function(x,...) stats::quantile(x,0.75,...)
quantile95 <- function(x,...) stats::quantile(x,0.95,...)

#'@param d the default value
#'@param y a list of alternative values
#'@param z a list of filters
#'#'@param na what to use if the both d and y filter is empty
GetSelection <- function(d, y,z = y, na = z) {
  if(sum(d %in% z)>0) return(d[d %in% z])
  u <- unlist(lapply(y,function(x) x[x %in% z]))
  if(length(u)>0) return(u)
  na
}

# Auto-detect date and time
#' @param x character vector with dates.
#' @return a POSTIXct object with dates.
toDateTime <- function(x) {
  # may want to do only unique cases and re-join after
  
  # v0.0.1 force (windows script)
  a <- x
  a <- as.POSIXct(x = strptime( x, format = "%d/%m/%Y  %I:%M %p"))
  a[is.na(a)] <- as.POSIXct(x = strptime( x[is.na(a)], format = "%Y-%m-%d %H:%M:%S"))
  a
  
  # v.0.1.0 - detect from first 100 items
  ainit <- data.table::data.table(sample = x[1:min(100,length(x))])

  prefix <- "" # "lubridate::" # do.call requires package to be attached. See DESCRIPTION.
  # 1 - y, 2 - ym/my,md/dm,/
  fcs <- c("ymd","ydm","mdy","myd","dmy","dym",
           "ymd_h","ydm_h","mdy_h","dmy_h",
           "ymd_hm","ydm_hm","mdy_hm","dmy_hm",
           "ymd_hms","ydm_hms","mdy_hms","dmy_hms",
           "my","ym","yq" 
           )
  pFcs <- paste0(prefix,fcs)
  for(i in pFcs) ainit[, (i) := do.call(what = i,args = list(sample),quote = T)]
  
  allFormats <- ainit[,!"sample"][,lapply(.SD,function(y) sum(!is.na(y))),] # calculate formats
  
  # if(max(ainit[,apply(.SD,2,function(z) sum(!is.na(z))),.SDcols=c(pFcs)])==dim(ainit)[1]) { 
    # good (or most common) format
    fcsFinal <- names(which.max(allFormats))[1] # find the best format for the sample
    a[is.na(a)] <- as.POSIXct( x = do.call(what = fcsFinal, args = list(x[is.na(a)]),quote = T) )
  # }
    # exceptional cases (doesn't always work esp with small sample sizes)
    #fcsCandidates <- names(allFormats)[which(allFormats>0)] # for 100 samples, the ones found
    ana <- data.table::data.table(sample = x[is.na(a)])
    for(i in pFcs) ana[, (i) := do.call(what = i,args = list(sample),quote = T)]
    #iris[, maximum_element := do.call(pmax, .SD), .SDcols = 1:4]
          
    ana[,val := 1 + min(which(!is.na(.SD))),by=c("sample"),.SDcols = c(pFcs)] # first match accepted
    a[is.na(a)] <- ana[,apply(.SD,1,function(z) z[as.numeric(z["val"])] )] # return first match
    
    # future functionality for machine dates (# seconds or days since 1970-01-01 00:00:00 UTC) 
  if(sum(is.na(a))==length(x)){ # most likely nto a date object, return original values
    warning("column with a date label is most likely not a date value, returning original column")
    x
  } else{
    a 
  }
}

#' classifies the names of the input data table by class
#'@param y the data table.
#'@return a list of numeric, strings, and date column names.
#' $cols_numeric
#' $cols_string
#' $cols_date
GetO2Names <- function(y){
  yNames <- list()
  temp_1 <- unique(names(y)[sapply(y, is.numeric)])
  yNames$cols_numeric <- temp_1[order(temp_1)]
  temp_2 <- unique(names(y)[sapply(y, function(x) is.character(x) | is.logical(x))])
  yNames$cols_string <- temp_2[order(temp_2)]
  yNames$cols_date <- names(y)[grepl("date",names(y),ignore.case = TRUE)]
  return(yNames)  
}

#' function that adds visual HTML tags to each row entry
#' add icon tags to a data.table,
#' add background tag to a data.table,
#' based on file type.
#' note this is only useful to sort out information visually. 
#' @param x the data table, should have the following column names
#'        "Extension" name of the extension - character
#'        "Type": "File" or "Folder". - character
#'        "parentName"
#'        "pathString"
#'        "Owner" 
#' @return x with new columns
#'        "ext" lower case extension for files - character
#'        "ico" extension icon (varies by Extension) - character
#'        "link" icon, full file-link and name of the document (with extension) - character
#'        "parentName" parentName with HTML link - character
#'        "pathString" pathString with HTML link - character
#'        "bg_clr" background color for folders (light-brown) - character
#'        "OwnerEmail" link to GEDS email (in development) - (probably character).
addIcon <- function(x = NULL){
  # note there are 330 extensions, and counting!! so we are only going to do the top 5.
  
  # Note that we store the file extensions in the case sensitive manner, but it might not be the case in reality.
  # file-pdf : pdf,
  # file-word : docx, doc,
  # link: external-link-square-alt
  if(!exists("fileIcons")) load("data/fileIcons.rda")
  if(is.null(x)) x <- data.table::copy(O2Clean[,data.table::first(.SD,1),by=Type])
  print("addIcon")
  if('Extension' %in% names(x)) x[,ext := tolower(Extension)]
  if('ext' %in% names(x)) x <- fileIcons[x,on=c("ext")]
  
  if(sum(!c('Type','pathString','Extension','ico','Name') %in% names(x)) == 0) {
  x[Type %in% c("File"), link := paste('<a href="',"file:", gsub('\\\\', '/', pathString),'.',Extension, '">',ico,' ', Name, '</a>', sep = "") ]
  x[Type %in% c("Folder"), link := paste('<a href="',"file:", gsub('\\\\', '/', pathString),'/', '">',ico,' ', Name, '</a>', sep = "") ]
  }
  
  # example: shiny::a(href = c("https://drive.google.com/"), shiny::icon('google-drive'))
  
  if(sum(!c('Type','ico') %in% names(x))==0) {
    x[Type %in% c("Folder"), ico := as.character(shiny::icon("folder"))]
    x[ico %in% NA & Type %in% c("File"), ico := as.character(shiny::icon("file"))]
  }
  print("finish addIcon")
  return(x)
}