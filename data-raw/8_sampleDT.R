## code to prepare `sampleDT` dataset goes here
sampleDT <- list()
temploc <- gsub("\\\\","/",tempdir())
curl::curl_download(url = "https://codeload.github.com/microsoft/ML-For-Beginners/zip/refs/heads/main",
                    destfile = paste0(temploc,"/","ML.zip"))
zip::unzip(zipfile = paste0(temploc,"/","ML.zip") )
# run the script and save CSV as ML-fr

sampleDT$MLFB <- data.table::fread("data-raw/ML-for-beginners.csv")
sampleDT$MLFB[, Owner := "microsoft"] # for sample only
sampleDT$MLFB[,DateCreated := DateWritten] # for sample only
sampleDT$MLFB[,DateAccessed := DateWritten] # for sample only
sampleDT$MLFB[, pathString := gsub("\\\\","/",pathString)]
sampleDT$MLFB[, parentName := gsub("\\\\","/",parentName)]

sampleDT$MLFB[,pathString := gsub("C:/","https://github.com/microsoft/ML-For-Beginners/",pathString)]

sampleDT$MLFB[,parentName := gsub("C:/","https://github.com/microsoft/ML-For-Beginners/",parentName)]
sampleDT$MLFB[,link := gsub("file:C:/","https://github.com/microsoft/ML-For-Beginners/",link)]

sampleDT$mtcars <- data.table::data.table(mtcars)
sampleDT$mtcars[,model := gsub("^[A-z0-9]* ","",rownames(mtcars))]
sampleDT$mtcars[,make := gsub(" [A-z0-9]*","",rownames(mtcars))]
data.table::setcolorder(sampleDT$mtcars,c("make","model"))

usethis::use_data(sampleDT, overwrite = TRUE)
