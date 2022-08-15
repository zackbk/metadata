## code to prepare `colClass` dataset goes here
colClass <- list("character" = c("parentName","DateWritten","DateCreated","DateAccessed","Owner","Name","Extension","Type","pathString"),
                 "integer" = c("parentID","ID","CharacterLength","Level","TotalFileCount","DirectFileCount"),
                 "numeric" = c("TotalByteSize","DirectByteSize"))


colOrder <- c("link","Owner","Extension","chng_type","Level","DateCreated","DateAccessed","DateWritten",
  "TotalByteSize","DirectByteSize","DirectFileCount","CharacterLength",
  "pathString",
  "parentName","ID",'parentID',
  "TotalFileCount",'bg_clr','ico','ext','fileType'
)

usethis::use_data(colClass,colOrder, overwrite = TRUE)
