# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp(appName = "DriveIndexTool_zhVqFYg6LV947ZFEJfHTpyKdQQ")
# Or use the blue button on top of this file
pkgload::load_all(export_all = FALSE,helpers = FALSE,attach_testthat = FALSE)
options( "golem.app.prod" = TRUE,shiny.maxRequestSize = 1000*1024^2)
ShareDriveApp::run_app() # add parameters here (if any)
