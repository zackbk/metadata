## code to prepare `3_fileIcons` dataset goes here
fileIcons <- data.table::fread("data-raw/fileIcons.csv")
fileIcons <- fileIcons[,lapply(.SD,function(x) unlist(strsplit(tolower(x),split = ","))),by=.(fileIcon,bg_clr),.SDcols=c('ext')]
fileIcons[,ico := sapply(fileIcons$fileIcon,function(z){ 
  as.character(shiny::icon(z))
})]
fileIcons <- fileIcons[,-"fileIcon"][,data.table::first(.SD,1),by=c("ext")]
usethis::use_data(fileIcons, overwrite = TRUE)

