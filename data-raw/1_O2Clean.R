

## code to prepare `O2Clean` dataset goes here

# Shared Drive File Folder Indexing (One time)
# https://ss64.com/nt/dir.html

# STEP 0: RUN SHELL COMMAND:
## Note: this takes a long time (30 mins). Only run once per day, at most.

#shell.exec(file = "data-raw/ListAllShareDriveFoldersFiles.bat")
## GetO2
source("R/fct_GetO2.R")
O2Clean <- GetO2(myDir="../SDA-temp/temp/",FName=c("FileFolderListvW_022020.txt","FileFolderListvC_102020.txt","FileFolderListvA_052021.txt"))
O2Clean[,chng_type := "="]
O2Clean[,chng_sum := "0=1"]
O2Empty <- O2Clean[FALSE,]
usethis::use_data(O2Empty, overwrite = TRUE)

