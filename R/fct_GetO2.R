#' GetO2 
#'
#' @description Accepts as input the windows index, and provides as output an organized data table 
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


#'@param myDir ?.
#'@param FName prefix for the file name - typically this is the name of the mapped drive, "FileFolderListv".
#'@param WCA last written, creation, last accessed.
#'@param temploc ?.
#'@return O2Clean w
GetO2 <- function(myDir = NULL, FName = NULL, WCA = NULL, temploc = NULL, recursiveSum = FALSE) {
  # initialise libraries
  # library(bit64)
  # library(stringr)
  # library(data.table)
  # library(plyr)
  # library(dplyr)
  print("Run GetO2")
  if(is.null(temploc)) temploc <- gsub("\\\\","/",tempdir())
  O2Tables <- c()
  if(is.null(FName)) {
    FName <- myDir
  } else if (length(FName)>1 & length(myDir)==1 & sum(grepl("/|\\\\",FName))==0){
    myDir <- paste0(myDir,ifelse(grepl("/$|\\\\$",myDir),"","/"),FName)
  }
  
  if(is.null(WCA)){
    if(is.null(myDir)){
      #WCA <- paste0(c("W","C","A"),sprintf("_%02d%4d",data.table::month(Sys.Date()), data.table::year(Sys.Date())))
      error("GetO2 failed to find directory input")
    } else{
      WCA <- rep("W",length(FName))
      WCA[grepl("vC_[0-9]{0,8}\\.txt$",FName,ignore.case = TRUE)] <- "C"
      WCA[grepl("vW_[0-9]{0,8}\\.txt$",FName,ignore.case = TRUE)] <- "W"
      WCA[grepl("vA_[0-9]{0,8}\\.txt$",FName,ignore.case = TRUE)] <- "A"
    }
  }
  print("WCA")
  print(WCA)  
  # Step 1: Clean Data
  # A is accessed, we don't typically use.
  
  for(i in 1:length(myDir)) { # last written, last created, last accessed
    print(WCA[i])
    
    O2_original <- base::readLines(myDir[i])
    
    O2 <- O2_original[O2_original!=""]
    
    O2Dir <- substring(O2,15,nchar(O2))[substring(O2,1,14)==" Directory of "] #extract all directory names
    #O2Dir <- stringr::str_replace_all(string = O2Dir, pattern = fixed("H:\\"),replacement= stringr::fixed( "\\\\?????\\shares\\??\\???\\")) 
    O2DirCount <- (data.frame("z"= O2) %>% dplyr::mutate(mydir=cumsum(substring(z,1,14)==" Directory of ")))$mydir # link files to directories
    O2NodeID <- 1:length(O2Dir)
    
    O2Property <- substring(O2,1,1)==" " # if first letter is "blank" then it's either "directory of" or lists file numbers
    O2DirNums <- as.numeric(substring(O2[O2Property][stringr::str_which(O2[O2Property],"File\\(s\\)")],1,17))  # num of files in directory  (NOT in Sub directories)
    O2DirSize <- as.numeric(substring(O2[O2Property][stringr::str_which(O2[O2Property],"File\\(s\\)")],26,40)) # size of files in directory (NOT in Sub directories)
    
    
    O2FileSubDirInfo <- O2[!O2Property] # lines that might be files
    O2ParentID <- O2DirCount[!O2Property]
    
    O2IsFile <- !grepl("<DIR>",substring(O2FileSubDirInfo,21,39))      # != "    <DIR>          "
    SubDirOnly <- function(y = O2FileSubDirInfo[!O2IsFile]) substring(y,63,nchar(y))!="." & substring(y,63,nchar(y))!=".."
    
    O2FileInfo <- O2FileSubDirInfo[O2IsFile] # keep files
    O2SubDirInfo <- O2FileSubDirInfo[!O2IsFile][SubDirOnly()] # keep subdirectories
    O2FileParentID <- O2ParentID[O2IsFile] # Parent Directory ID
    O2SubDirParentID <- O2ParentID[!O2IsFile][SubDirOnly()]  # Parent Directory ID
    
    O2FileID <- seq(max(O2NodeID)+1,max(O2NodeID)+length(O2FileInfo)) # file ID
    O2FileDateTime <- substring(O2FileInfo,1,20)
    O2FileBytes <- as.numeric(gsub(pattern = ",","",substring(O2FileInfo,21,39)))
    O2FileOwner <- trimws(substring(O2FileInfo,40,62))
    O2FileNameExt <- substring(O2FileInfo,63,nchar(O2FileInfo))
    O2FileExt <- stringi::stri_extract_last(str = O2FileNameExt,regex = "\\.[0-9A-z]*")
    O2FileExt <- substring(O2FileExt,2,nchar(O2FileExt))
    O2FileName <- substring(O2FileNameExt,1,nchar(O2FileNameExt)-nchar(O2FileExt)-1)
    O2FileNChars <- nchar(paste0(O2Dir[O2FileParentID],O2FileNameExt))
    O2FileLevel <- stringr::str_count(string = O2Dir[O2FileParentID], pattern="\\\\")
    
    O2SubDirDateTime <- as.POSIXct(x = strptime( substring(O2SubDirInfo,1,20), format = "%d/%m/%Y  %I:%M %p", tz =  "EST" ))
    O2SubDirOwner <- substring(O2SubDirInfo,40,62)
    O2SubDirName <- substring(O2SubDirInfo,63,nchar(O2SubDirInfo))
    O2SubDirID <- match(paste0(O2Dir[O2SubDirParentID],"\\",O2SubDirName),table=O2Dir)
    O2SubDirLevel <- stringr::str_count(string = O2Dir[O2SubDirParentID], pattern="\\\\") # Relative SubDirectory Level
    
    O2SubDirBytes <- O2DirSize[match(paste0(O2Dir[O2SubDirParentID],"\\",O2SubDirName),table=O2Dir)]
    O2SubDirBytes[is.na(O2SubDirBytes)] <- 0
    O2SubDirNFiles <- O2DirNums[match(paste0(O2Dir[O2SubDirParentID],"\\",O2SubDirName),table=O2Dir)] # number of direct files
    O2SubDirNFiles[is.na(O2SubDirNFiles)] <- 0
    
    O2FileFullPath <- paste0(O2Dir[O2FileParentID],"\\",O2FileName)
    O2SubDirFullPath <- paste0(O2Dir[O2SubDirParentID],"\\",O2SubDirName)
    
    
    O2Data <- plyr::rbind.fill(tryCatch(data.frame("parentName" = O2Dir[O2FileParentID],
                                                   "parentID" = O2FileParentID,
                                                   "DateTime" = O2FileDateTime,
                                                   "Owner" = O2FileOwner,
                                                   "Name" = O2FileName,
                                                   "ID" = O2FileID,
                                                   "Extension" = O2FileExt,
                                                   "CharacterLength" = O2FileNChars,
                                                   "Level" = O2FileLevel,
                                                   "Type" = "File",
                                                   "TotalByteSize" = O2FileBytes,
                                                   "TotalFileCount" = 1,
                                                   "pathString" = O2FileFullPath
    ),error = function(e) {
      data.frame()
    }), tryCatch(data.frame('parentName' = O2Dir[O2SubDirParentID],
                            "parentID" = O2SubDirParentID,
                            "DateTime" = O2SubDirDateTime,
                            "Owner" = O2SubDirOwner,
                            "Name" = O2SubDirName,
                            "ID" = O2SubDirID,
                            "Level" = O2SubDirLevel,
                            "DirectFileCount" = O2SubDirNFiles, # O2DirNums
                            "DirectByteSize" = O2SubDirNFiles, # Direct Files
                            "Type" = "Folder",
                            "TotalByteSize" = 0, # Must be cumulative (will be recursively done in next step)
                            "TotalFileCount" = 0,
                            "pathString" = O2SubDirFullPath),error = function(e) {
                              data.frame()
                            })) # Must be cumulative (will be recursively done in next step)
    
    
    DateTime <- ifelse(substring(WCA[i],1,1)=="W","DateWritten",ifelse(substring(WCA[i],1,1)=="C","DateCreated","DateAccessed"))
    print("DateTime")
    print(DateTime)
    names(O2Data)[names(O2Data) %in% "DateTime"] <- DateTime
    O2Data[,paste0(DateTime,"IndexDate")] <- stringr::str_extract(FName[i],"[0-9]{8}") # #Sys.time() # NEW
    data.table::fwrite(O2Data,paste0(temploc,"/clean_datav",substring(WCA[i],1,1),".csv"))
    O2Tables <- c(O2Tables,paste0(temploc,"/clean_datav",substring(WCA[i],1,1),".csv"))
    rm(O2_original,O2,O2Dir,O2DirCount,O2NodeID,O2Property,O2DirNums,O2DirSize,O2FileSubDirInfo,O2ParentID,
       O2IsFile,O2FileInfo,O2SubDirInfo,O2FileParentID,O2SubDirParentID,O2FileDateTime,O2FileBytes,O2FileOwner,
       O2FileNameExt,O2FileExt,O2FileName,O2FileNChars,O2FileLevel,O2SubDirDateTime,O2SubDirOwner,
       O2SubDirName,O2SubDirLevel,O2SubDirBytes,O2SubDirNFiles,O2FileFullPath,O2SubDirFullPath,O2Data)
  }
  
  # (Optional) Step 2: Recursive Sum of Levels: not currently used
  RecursiveSum <- function(x) {
    # Required for function to work
    print(paste("init",Sys.time()))
    x$TotalByteSize[x$Type=="File" & !is.na(x$DirectByteSize) & (x$DirectByteSize>0)] <- 
      x$DirectByteSize[x$Type=="File" & !is.na(x$DirectByteSize) & (x$DirectByteSize>0)]
    x$TotalFileCount[x$Type=="File" & !is.na(x$DirectFileCount) & (x$DirectFileCount>0)] <- 
      x$DirectFileCount[x$Type=="File" & !is.na(x$DirectFileCount) & (x$DirectFileCount>0)]
    # "Direct" columns for files are NA
    x$DirectByteSize[x$Type=="File"] <- NA
    x$DirectFileCount[x$Type=="File"] <- NA
    # "Direct" columns for folders are >=0
    x$DirectByteSize[x$Type=="Folder" & !is.na(x$TotalByteSize) & (x$TotalByteSize>0)] <- 
      x$TotalByteSize[x$Type=="Folder" & !is.na(x$TotalByteSize) & (x$TotalByteSize>0)]
    x$DirectFileCount[x$Type=="Folder" & !is.na(x$TotalFileCount) & (x$TotalFileCount>0)] <- 
      x$TotalFileCount[x$Type=="Folder" & !is.na(x$TotalFileCount) & (x$TotalFileCount>0)]
    # "Total" columns for folders are ZERO
    x$TotalByteSize[x$Type=="Folder"] <- 0
    x$TotalFileCount[x$Type=="Folder"] <- 0
    
    print(paste("start",Sys.time()))
    for(j in seq(max(x$Level) - 1, min(x$Level),-1)) { # start from lowest level work up.
      cat(paste('\r',max(x$Level)-1,"=>",j,"=>",min(x$Level),": started" ))
      # x <- x %>% dplyr::group_by(`parentID`) %>%
      #   dplyr::mutate("TotalByteSize" = sum(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalByteSize[x$Level==(j+1)], 0 ), na.rm=T ) ,
      #                 "TotalFileCount" = sum(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalFileCount[x$Level==(j+1)], 0), na.rm=T ) #,
      #                 #"AvgByteSize" = mean(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalFileCount[x$Level==(j+1)], 0), na.rm=T ),
      #                 #"AvgCharacterLength" = mean(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalFileCount[x$Level==(j+1)], 0), na.rm=T )
      #   ) %>% dplyr::ungroup()
      
      y <- data.table::copy(x)
      y <- y[Level == j + 1, list(TBS = sum(TotalByteSize,na.rm=T),TFC=sum(TotalFileCount,na.rm=T)),by = c("parentName")]
      x[y,`:=` ("TotalByteSize" = TBS,"TotalFileCount" = TFC) ,on=c("parentName==parentName")]
      
      # x[,`:=` ("TotalByteSize" = sum(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalByteSize[x$Level==(j+1)], 0 ), na.rm=T ),
      #          "TotalFileCount" = sum(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalFileCount[x$Level==(j+1)], 0), na.rm=T )),by=c("parentID")]
      cat(paste('\r',max(x$Level)-1,"=>",j,"=>",min(x$Level),": fin'd" ))
    }
    print(paste("finish", Sys.time()))
    return(x)
  }
  # step 3 : Rerun this and subsequent steps to update file icons etc.
  # Step 4: Combine multiple time-stamps
  
  # Reload data
  colClass <- list("character" = c("parentName","DateWritten","DateCreated","DateAccessed","Owner","Name","Extension","Type","pathString"),
                   "integer" = c("parentID","ID","CharacterLength","Level","TotalFileCount","DirectFileCount"),
                   "numeric" = c("TotalByteSize","DirectByteSize"))
  
  #O2Tables <- list.files(path = temploc,pattern = "clean_datav",full.names = TRUE)
  
  for(i in seq_along(O2Tables)) {
    if(i == 1){
      O2Clean <- data.table::fread(O2Tables[i], data.table = T, colClasses = colClass)
    } else {
      O2temp <- data.table::fread(O2Tables[i], data.table = T, colClasses = colClass)
      onCols <- names(O2temp)[names(O2temp) %in% names(O2Clean)]
      O2Clean <- data.table::rbindlist(l = list(O2temp[O2Clean,on=c(onCols)],
                                                O2temp[!O2Clean,on=c(onCols)]),
                                       use.names = TRUE,
                                       fill = TRUE)
    }
    
  }
  
  # if(recursiveSum==TRUE) O2Clean <- RecursiveSum(O2Clean)
  
  # Step 5: Clean up the formatting
  if(!exists("toDateTime") | !exists("addIcon")) source("R/fct_helpers.R") # functions in fct_helpers
  
  # toDateTime <- function(x) {
  #   a <- as.POSIXct(x = strptime( x, format = "%d/%m/%Y  %I:%M %p"))
  #   a[is.na(a)] <- as.POSIXct(x = strptime( x[is.na(a)], format = "%Y-%m-%d %H:%M:%S"))
  #   return(a)
  # }
  
  # if("DateAccessed" %in% names(O2Clean)) O2Clean[, DateAccessed := toDateTime(O2Clean$DateAccessed)]
  # if("DateWritten" %in% names(O2Clean)) O2Clean[, DateWritten := toDateTime(O2Clean$DateWritten)]
  # if("DateCreated" %in% names(O2Clean)) O2Clean[, DateCreated := toDateTime(O2Clean$DateCreated)]
  
  date_cols <- grep("date",names(O2Clean),ignore.case = TRUE)
  for(dc in date_cols) O2Clean[, (names(O2Clean)[dc]) := toDateTime(O2Clean[[names(O2Clean)[dc]]])]
  
  if("Owner" %in% names(O2Clean))  O2Clean[, Owner := gsub("\\\\","/",Owner)]
  
  colOrder <- c("link","Owner","Extension","Level","DateCreated","DateAccessed","DateWritten",
                "TotalByteSize","DirectByteSize","DirectFileCount","CharacterLength",
                "pathString",
                "parentName","ID",'parentID',
                "TotalFileCount",'bg_clr','ico','ext'
  )
  
  O2Clean <- addIcon(O2Clean)
  data.table::setcolorder(O2Clean, 
                          neworder = colOrder[colOrder %in% names(O2Clean)])
  
  
  
  print("End GetO2")
  return(O2Clean)
}
