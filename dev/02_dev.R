# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

## Dependencies ----
## Add one line by package you want to add as dependency
#usethis::use_package("thinkr" )
usethis::use_package("DT")
usethis::use_package("magrittr")
usethis::use_package("data.tree")
usethis::use_package("data.table")
usethis::use_package("plyr")
usethis::use_package("dplyr")
usethis::use_package("stringi")
usethis::use_package("stringr")
usethis::use_package("shinyWidgets")
usethis::use_package("ggplot2")
usethis::use_package("ggrepel")
usethis::use_package("class") # simple regression, graphics
usethis::use_package("rclipboard")
usethis::use_package("scales")
usethis::use_package("shinymaterial")
usethis::use_package("shinyalert")
usethis::use_package("bit64") # for integer bytes to work properly
# usethis::use_package("ggiraph")
# usethis::use_package("plotly")
## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "1_body" ) # Name of the module
golem::add_module( name = "2_search" ) # Name of the module
golem::add_module( name = "2_2_subctlr" ) # Name of the module
golem::add_module( name = "3_view" ) # Name of the module
golem::add_module( name = "4_engine" ) # Name of the module
golem::add_module( name = "4_1_script" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "SearchModel" ,open = TRUE) 
golem::add_fct( "SortView"   ,open = TRUE)
golem::add_fct( "GetO2"   ,open = TRUE)
golem::add_fct( "GetDT"   ,open = TRUE)
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "1_O2Clean", open = TRUE )
usethis::use_data_raw( name = "2_T2", open = TRUE )
usethis::use_data_raw( name = "3_fileIcons", open = TRUE )
usethis::use_data_raw( name = "4_O2Names", open = TRUE )
usethis::use_data_raw( name = "5_timeInit", open = TRUE )
usethis::use_data_raw( name = "6_colClass", open = TRUE )
usethis::use_data_raw( name = "7_rmdFile", open = TRUE )
usethis::use_data_raw( name = "8_sampleDT", open = TRUE )
## Tests ----
## Add one line by test you want to create
usethis::use_test( "app" )

# Documentation

## Vignette ----
usethis::use_vignette("MetadataApp")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

