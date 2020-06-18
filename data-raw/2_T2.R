T2 <- O2Clean[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = Extension, .SDcols = c("TotalByteSize", "TotalFileCount")][order(-TotalFileCount)]

usethis::use_data(T2, overwrite = TRUE)
