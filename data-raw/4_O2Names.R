if(!exists("O2Clean")) load("data/O2Empty.rda")
if(!exists("GetO2Names")) source('R/fct_helpers.R')

O2Names <- GetO2Names(O2Empty)

usethis::use_data(O2Names, overwrite = TRUE)
