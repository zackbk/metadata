if(!exists("O2Clean")) load("data/O2Clean.rda")
T2 <- O2Clean[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = ext, .SDcols = c("TotalByteSize", "TotalFileCount")][
  order(-TotalByteSize*TotalFileCount),
]

T3 <- O2Clean[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = Owner, .SDcols = c("TotalByteSize", "TotalFileCount")][
  order(-TotalByteSize*TotalFileCount),
]

usethis::use_data(T2,T3, overwrite = TRUE)
