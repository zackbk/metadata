## code to prepare `rmdFile` dataset goes here
rmdFile <- base::readLines("data-raw/Export.Rmd")

usethis::use_data(rmdFile, overwrite = TRUE)
