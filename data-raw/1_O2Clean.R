

## code to prepare `O2Clean` dataset goes here

# Shared Drive File Folder Indexing (One time)
# https://ss64.com/nt/dir.html

# STEP 0: RUN SHELL COMMAND:
## Note: this takes a long time (30 mins). Only run once per day, at most.

#shell.exec(file = "data-raw/ListAllShareDriveFoldersFiles.bat")



GetO2 <- function(myDir = NULL, FName = "FildeFolderListv", WCA = NULL, temploc = NULL) {
  # initialise libraries
library(bit64)
library(stringr)
library(data.table)
library(plyr)
library(dplyr)
  if(is.null(temploc)) temploc <- gsub("\\\\","/",tempdir())
  if(is.null(WCA)){
    WCA <- paste0(c("W","C","A"),sprintf("_%02d%4d",data.table::month(Sys.Date()), data.table::year(Sys.Date())))
  }
  
# Step 1: Clean Data
 # A is accessed, we don't typically use.
for(wca in WCA) { # last written, last created, last accessed
  cat(wca)
  if(!is.null(myDir)){
    O2_original <- base::readLines(paste0(myDir,FName,wca,".txt"))  
  } else if(file.exists(paste0("//ncr.int.ec.gc.ca/shares/O/OGAED/",FName,wca,".txt"))) {
    O2_original <- base::readLines(paste0("//ncr.int.ec.gc.ca/shares/O/OGAED/",FName,wca,".txt"))  
  } else if(file.exists(paste0(temploc,"/",FName,wca,".txt"))) {
    O2_original <- base::readLines(paste0(temploc,"/",FName,wca,".txt"))
  } else{
    O2_original <- base::readLines(paste0("data-raw/archive/",FName,wca,".txt"))
  }
  O2 <- O2_original[O2_original!=""]
  
  O2Dir <- substring(O2,15,nchar(O2))[substring(O2,1,14)==" Directory of "] #extract all directory names
  O2Dir <- stringr::str_replace_all(string = O2Dir, pattern = fixed("H:\\"),replacement= stringr::fixed( "\\\\ncr.int.ec.gc.ca\\shares\\O\\OGAED\\")) 
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
  
  
  
  O2Data <- plyr::rbind.fill(data.frame("parentName" = O2Dir[O2FileParentID],
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
  ),
  
  data.frame('parentName' = O2Dir[O2SubDirParentID],
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
             "pathString" = O2SubDirFullPath)) # Must be cumulative (will be recursively done in next step)
  
  DateTime <- ifelse(substring(wca,1,1)=="W","DateWritten",ifelse(substring(wca,1,1)=="C","DateCreated","DateAccessed"))
  names(O2Data)[names(O2Data) %in% "DateTime"] <- DateTime
  O2Data[,paste0(DateTime,"IndexDate")] <- substring(wca,3) #Sys.time() # NEW
  data.table::fwrite(O2Data,paste0(temploc,"/clean_datav",substring(wca,1,1),".csv"))
  
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
    x <- x %>% dplyr::group_by(`parentID`) %>%
      dplyr::mutate("TotalByteSize" = sum(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalByteSize[x$Level==(j+1)], 0 ), na.rm=T ) ,
             "TotalFileCount" = sum(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalFileCount[x$Level==(j+1)], 0), na.rm=T ) ,
             "AvgByteSize" = mean(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalFileCount[x$Level==(j+1)], 0), na.rm=T ),
             "AvgCharacterLength" = mean(ifelse(`parentID` == x$parentID[x$Level==(j+1)], x$TotalFileCount[x$Level==(j+1)], 0), na.rm=T )
      ) %>% dplyr::ungroup()
    cat(paste('\r',max(x$Level)-1,"=>",j,"=>",min(x$Level),": fin'd" ))
  }
  print(paste("finish", Sys.time()))
  return(x)
}
#O2Clean <- data.table::fread(temploc,"/clean_data.csv")
#temp <- RecursiveSum(O2Clean)
#usethis::use_data(O2Clean, overwrite = TRUE)

# step 3 : Rerun this and subsequent steps to update file icons etc.

toDateTime <- function(x) {
  a <- as.POSIXct(x = strptime( x, format = "%d/%m/%Y  %I:%M %p"))
  a[is.na(a)] <- as.POSIXct(x = strptime( x[is.na(a)], format = "%Y-%m-%d %H:%M:%S"))
  return(a)
}

# Step 4: Combine multiple time-stamps

#  Summarise at a level of your choice
colClass <- list("character" = c("parentName","DateWritten","DateCreated","DateAccessed","Owner","Name","Extension","Type","pathString"),
                 "integer" = c("parentID","ID","CharacterLength","Level","TotalFileCount","DirectFileCount"),
                 "numeric" = c("TotalByteSize","DirectByteSize"))

O2Tables <- list.files(path = temploc,pattern = "clean_datav",full.names = TRUE)

for(i in seq_along(O2Tables)) {
  if(i == 1){
    O2Clean <- fread(O2Tables[i], data.table = T, colClasses = colClass)
  } else {
    O2temp <- fread(O2Tables[i], data.table = T, colClasses = colClass)
    onCols <- names(O2temp)[names(O2temp) %in% names(O2Clean)]
    O2Clean <- rbindlist(l = list(O2temp[O2Clean,on=c(onCols)],
                                  O2temp[!O2Clean,on=c(onCols)]),
                         use.names = TRUE,
                         fill = TRUE)
  }
  
}

# O2List <- list(
#   data.table::fread(paste0(temploc,"/clean_datavW.csv"),data.table = T, colClasses = colClass),
#   data.table::fread(paste0(temploc,"/clean_datavC.csv"),data.table = T, colClasses = colClass),
#   data.table::fread(paste0(temploc,"/clean_datavA.csv"),data.table = T, colClasses = colClass))
# onCols <- names(O2List[[1]])[names(O2List[[1]]) %in% names(O2List[[2]]) & names(O2List[[1]]) %in% names(O2List[[3]])]
# onOrder <- order(c(dim(O2List[[1]])[1],dim(O2List[[2]])[1],dim(O2List[[3]])[1]))
# 
# O2Clean <- O2List[[onOrder[1]]][O2List[[onOrder[2]]][O2List[[onOrder[3]]],on= c(onCols)],on= c(onCols)]

#data.table::fwrite(O2Clean,paste0(temploc,"/clean_datavCWA.csv"))
O2Clean[, DateAccessed := toDateTime(O2Clean$DateAccessed)]
O2Clean[, DateWritten := toDateTime(O2Clean$DateWritten)]
O2Clean[, DateCreated := toDateTime(O2Clean$DateCreated)]
O2Clean[, Owner := gsub("\\\\","/",Owner)]

source("data-raw/data_fct_addIcon.R")
O2Clean <- addIcon(O2Clean)
data.table::setcolorder(O2Clean, 
                        neworder = c("link","Owner","Extension","Level","DateCreated","DateAccessed","DateWritten",
                                     "TotalByteSize","DirectByteSize","DirectFileCount","CharacterLength",
                                     "pathString",
                                     "parentName","ID",'parentID',
                                     "TotalFileCount",'bg_clr','ico','ext'
                        ))
return(O2Clean)
}


O2Clean <- GetO2(myDir = NULL,FName = "FildeFolderListv", WCA = c("W_022020","C_102020","A_052021"))
usethis::use_data(O2Clean, overwrite = TRUE)

