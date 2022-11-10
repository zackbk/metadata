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
      shinydashboard::dashboardHeader(title = "Metadata lite v.0.1.0"),
      shinydashboard::dashboardSidebar(width = '300px',
                                       tags$style(
                                         "#sidebarItemExpanded {
            position: fixed;
            overflow: auto;
            max-height: 100vh;
            max-width: 300px;
            color:grey;
            text-align: center;
        }"), disable = FALSE,collapsed = FALSE,
                                       shinyWidgets::downloadBttn(outputId = "export",label = "V. export", style = "material-flat", color = "default", size = "md", block = FALSE),
                                       # shinydashboard::box(width = 6, collapsible = TRUE,title = "Search",collapsed = FALSE,status = 'success', solidHeader = TRUE,
                                       shinydashboard::box(width = 12, collapsible = TRUE,title = "I. Script Generator",collapsed = TRUE,status = 'primary', solidHeader = TRUE,background = 'navy',
                                                           shiny::helpText(icon('info'),"script only works on windows"),
                                                           mod_4_1_script_ui("4_1_script_ui_1"),
                                                           shiny::helpText(icon("info"),"use task scheduler to auto-run scripts")
                                                           
                                       ),
                                       shinydashboard::box(width = 12, collapsible = TRUE,title = "II. View Metadata",collapsed = FALSE,status = 'success', solidHeader = TRUE,background = 'teal',
                                                           mod_4_engine_ui("4_engine_ui_1"),
                                                           shiny::helpText("Converts dir output into a table"),
                                                           #shiny::helpText(icon("info"),"Data is temporarily stored on shinyapps.io servers"),
                                       ),
                                       shinydashboard::box(width = 12, collapsible = TRUE,title = "III. Search Metadata",collapsed = TRUE,status = 'primary', solidHeader = TRUE,background = 'navy',
                                                           mod_2_search_ui("2_search_ui_1")
                                       ),
                                       mod_2_2_subctlr_ui("2_2_subctlr_ui_1"),
                                       shiny::tags$text("Made with "),shiny::icon("heart",style = 'color: red;'),
                                       shiny::tags$metadata(' by Zack Kedida',style = 'max-width: 100px;'),
                                       shiny::tags$br(),
                                       shiny::helpText("Each Shiny application runs in its own protected environment and access is always SSL encrypted.
                                                       Session times out after 5 minutes of inactivity.")
                                       
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

