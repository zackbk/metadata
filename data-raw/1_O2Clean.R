

## code to prepare `O2Clean` dataset goes here

# Shared Drive File Folder Indexing (One time)
# https://ss64.com/nt/dir.html

# STEP 0: RUN SHELL COMMAND:
## Note: this takes a long time (30 mins). Only run once per day, at most.

#shell.exec(file = "data-raw/ListAllShareDriveFoldersFiles.bat")
## GetO2
source("R/fct_GetO2.R")
O2Clean <- GetO2(myDir = NULL,FName = "FileFolderListv", WCA = c("W_022020","C_102020","A_052021"))
O2Empty <- O2Clean[FALSE,]
usethis::use_data(O2Clean, O2Empty, overwrite = TRUE)

