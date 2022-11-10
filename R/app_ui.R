#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    
    shinydashboard::dashboardPage(#skin = "purple",
      shinydashboard::dashboardHeader(title = "Metadata lite"),
      shinydashboard::dashboardSidebar(width = '300px',
                                       tags$style(
                                         "#sidebarItemExpanded {
            position: fixed;
            overflow: auto;
            max-height: 100vh;
            max-width: 300px;
            color:olive;
            text-align: center;
            display: block;
        }"),
                                       shiny::tags$br(),
                                       shinydashboard::box(width = 12, collapsible = TRUE,title = "1. Generate",collapsed = TRUE,status = 'success', solidHeader = FALSE,background = 'olive',
                                                           shiny::helpText(icon('warning'),"only runs on windows"),
                                                           mod_4_1_script_ui("4_1_script_ui_1"),
                                                           shiny::helpText(icon("info"),"task scheduler can auto-run scripts")
                                                           
                                       ),
                                       
                                       shinydashboard::box(width = 12, collapsible = TRUE,title = "2. Upload",collapsed = TRUE,status = 'success', solidHeader = FALSE,background = 'olive',
                                                           mod_4_engine_ui("4_engine_ui_1"),
                                                           shiny::helpText("Convert script output into a table"),
                                                           #shiny::helpText(icon("info"),"Data is temporarily stored on shinyapps.io servers"),
                                       ),
                                       # shinydashboard::box(width = 12, collapsible = FALSE,background = 'olive',
                                       #                     title = 
                                       shinyWidgets::downloadBttn(outputId = "export",label = "HTML", icon = icon("file-export"),
                                                                  style = "material-flat", color = "success", size = "md",block = FALSE),
                                       # ),
                                       mod_2_2_subctlr_ui("2_2_subctlr_ui_1"),
                                       shinydashboard::box(width = 12, collapsible = FALSE,title = "3. Search", solidHeader = FALSE,background = 'olive',
                                                           mod_2_search_ui("2_search_ui_1")
                                       ),
                                       
                                       
                                       # shinydashboard::box(width = 6, collapsible = TRUE,title = "Search",collapsed = FALSE,status = 'success', solidHeader = TRUE,
                                       
                                       
                                       
                                       
                                       shiny::tags$text("Made with "),shiny::icon("heart",style = 'color: red;'),
                                       shiny::tags$metadata(' by Zack Kedida',style = 'max-width: 100px;'),
                                       shiny::tags$br(),
                                       shiny::helpText("Private app environment, SSL encrypted.")
                                       
      ),
      shinydashboard::dashboardBody(
        mod_3_view_ui("3_view_ui_1") 
        # )
        # )
      ),
      title = "Metadata"
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'MetadataApp'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    # shinyalert::useShinyalert(),
    shinyWidgets::useSweetAlert()
  )
}

