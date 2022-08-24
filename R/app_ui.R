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
        }"), disable = FALSE,collapsed = FALSE,
        
        # shinydashboard::box(width = 6, collapsible = TRUE,title = "Search",collapsed = FALSE,status = 'success', solidHeader = TRUE,
        # mod_2_controller_ui("2_controller_ui_1"),
        
        shinydashboard::box(width = 12, collapsible = TRUE,title = "I. Get windows dir script (optional)",collapsed = TRUE,status = 'primary', solidHeader = TRUE,background = 'navy',
                            mod_4_1_script_ui("4_1_script_ui_1"),
                            shiny::helpText(icon("info"),"use task scheduler to auto-run scripts")
        ),
        shinydashboard::box(width = 12, collapsible = TRUE,title = "II. Upload/Download",collapsed = TRUE,status = 'success', solidHeader = TRUE,background = 'teal',
                            mod_4_engine_ui("4_engine_ui_1"),
                            shiny::helpText("Use this module to convert the script data into a readable csv"),
                            shiny::helpText(icon("info"),"Data is temporarily stored on shinyapps.io servers"),
                            shiny::helpText("Each Shiny application runs in its own protected environment and access is always SSL encrypted"),
                            shiny::helpText(icon("circle-exclamation"),"shinyapps.io deletes all uploaded sessions if the connection times out (set to 5 minutes) or the session is closed")
        ),
        mod_2_2_subctlr_ui("2_2_subctlr_ui_1")
        
        ),
      shinydashboard::dashboardBody(
        mod_3_view_ui("3_view_ui_1") 
                          # )
        # )
      ),
      title = "Metadata"
    )
    
    
    # shiny::fluidPage(
    #   shinydashboard::dashboardPage (sidebarPanel = shiny::sidebarPanel(
    #     mod_2_controller_ui("2_controller_ui_1"),
    #     mod_2_2_subctlr_ui("2_2_subctlr_ui_1")
    #   ),mainPanel = shiny::mainPanel(
    #     mod_4_engine_ui("4_engine_ui_1"),
    #     mod_3_view_ui("3_view_ui_1")
    #   )
    #   )
    # )
    
    # shinymaterial::material_page(
    #   title = "MetadataApp",
    #   primary_theme_color = "#3700B3",
    #   secondary_theme_color = "#3700B3",
    #   background_color = "#FFFFFF",
    #   font_color = "#121212",
    #   nav_bar_color = "#aeea00",
    #   # mod_1_body_ui("1_body_ui_1")         # Note: no comma
    #   shinymaterial::material_row(
    #     shinymaterial::material_column(width = 6,
    #                                    shinymaterial::material_card(title = tags$p("Index"),
    #                                                                 mod_4_engine_ui("4_engine_ui_1")
    #                                    )
    #     ),
    #     shinymaterial::material_column(width = 6,
    #                                    shinymaterial::material_card(title = tags$p("Search"),
    #                                                                 mod_2_controller_ui("2_controller_ui_1"),
    #                                                                 mod_2_2_subctlr_ui("2_2_subctlr_ui_1")
    #                                    )
    #     )
    #   ),
    #   shinymaterial::material_card(title = tags$h3("Results"),
    #   mod_3_view_ui("3_view_ui_1")
    #   )
    # )
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

