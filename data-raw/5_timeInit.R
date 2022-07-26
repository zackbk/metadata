## code to prepare `timeInit` dataset goes here
timeInit <- as.POSIXct(Sys.Date())
usethis::use_data(timeInit, overwrite = TRUE)
