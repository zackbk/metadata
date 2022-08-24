## code to prepare `8_sampleDT` dataset goes here
sampleDT <- list()
sampleDT$mtcars <- data.table::data.table(mtcars)
sampleDT$mtcars[,model := gsub("^[A-z0-9]* ","",rownames(mtcars))]
sampleDT$mtcars[,make := gsub(" [A-z0-9]*","",rownames(mtcars))]
data.table::setcolorder(sampleDT$mtcars,c("make","model"))

usethis::use_data(sampleDT, overwrite = TRUE)
