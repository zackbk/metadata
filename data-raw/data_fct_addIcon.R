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