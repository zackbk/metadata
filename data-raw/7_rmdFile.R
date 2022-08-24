## code to prepare `7_rmdFile` dataset goes here
rmdFile <- base::readLines("R/Export.Rmd")

usethis::use_data(rmdFile, overwrite = TRUE)
