toDateTime <- function(x) {
  a <- as.POSIXct(x = strptime( x, format = "%d/%m/%Y  %I:%M %p"))
  a[is.na(a)] <- as.POSIXct(x = strptime( x[is.na(a)], format = "%Y-%m-%d %H:%M:%S"))
  a
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
  yNames$cols_date <- names(y)[data.table::like(names(y),"Date")]
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
  x[,ext := tolower(Extension)]
  x <- fileIcons[x,on=c("ext")]
  
  x[Type %in% c("File"), link := paste('<a href="',"file:", gsub('\\\\', '/', pathString),'.',Extension, '">',ico,' ', Name, '</a>', sep = "") ]
  x[Type %in% c("Folder"), link := paste('<a href="',"file:", gsub('\\\\', '/', pathString),'/', '">',ico,' ', Name, '</a>', sep = "") ]
  
  # example: shiny::a(href = c("https://drive.google.com/"), shiny::icon('google-drive'))
  
  x[Type %in% c("Folder"), ico := as.character(shiny::icon("folder"))]
  x[ico %in% NA & Type %in% c("File"), ico := as.character(shiny::icon("file"))]
  return(x)
}