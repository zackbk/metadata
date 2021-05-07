if(!exists("O2Clean")) load("data/O2Clean.rda")
O2Names <- list()
temp_1 <- unique(names(O2Clean)[sapply(O2Clean, is.numeric)])
O2Names$cols_numeric <- temp_1[order(temp_1)]
temp_2 <- unique(names(O2Clean)[sapply(O2Clean, function(x) is.character(x) | is.logical(x))])
O2Names$cols_string <- temp_2[order(temp_2)]
O2Names$cols_date <- names(O2Clean)[names(O2Clean) %like% "Date"]

usethis::use_data(O2Names, overwrite = TRUE)
