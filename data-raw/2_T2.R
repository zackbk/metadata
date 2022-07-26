if(!exists("O2Empty")) load("data/O2Empty.rda")
O2 <- O2Empty
T2 <- O2Empty[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = ext, .SDcols = c("TotalByteSize", "TotalFileCount")][
  order(-TotalByteSize*TotalFileCount),
]
T2Empty <- T2[FALSE,]

T3 <- O2Empty[Type == "File", lapply(.SD,function(x) sum(x,na.rm=T)), by = Owner, .SDcols = c("TotalByteSize", "TotalFileCount")][
  order(-TotalByteSize*TotalFileCount),
]
T3Empty <- T3[FALSE,]

usethis::use_data(T2Empty,T3Empty, overwrite = TRUE)