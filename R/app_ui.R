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
    # mod_1_body_ui("1_body_ui_1")
    
    # pageWithSidebar(headerPanel = headerPanel("Search OGAED Drive"),
    #                 sidebarPanel = sidebarPanel(
    #                   mod_2_controller_ui("2_controller_ui_1"),
    #                   width = 2
    #                 ),
    #                 mainPanel =  mainPanel(
    #                   mod_3_view_ui("3_view_ui_1")
    #                 )
    # 
    # )
    
    # shinymaterial::material_page(
    #   title = "ShinyDriveApp",
    #   shinymaterial::material_row(
    #     shinymaterial::material_column(
    #       width = 3,
    #       mod_2_controller_ui("2_controller_ui_1")
    #     ),
    #     shinymaterial::material_column(
    #       width = 9,
    #       mod_3_view_ui("3_view_ui_1")
    #     )
    #   )
    # )
    
    
    shinymaterial::material_page(
      title = "ShinyDriveApp",
      primary_theme_color = "#3700B3", 
      secondary_theme_color = "#3700B3",
      background_color = "#FFFFFF",
      font_color = "#121212",
      nav_bar_color = "#aeea00",
      mod_3_view_ui("3_view_ui_1"),
      shinymaterial::material_modal(modal_id = "welcome_text", button_text = "about",button_color = "blue 3",
                                    title = "Shared Drive Search Tool V0.2.0.0000", button_icon = "info", # help_outline
                                    shinymaterial::material_card(
                                      title = "How to / Help",
                                      shiny::helpText(
                                        icon("exclamation-triangle"),
                                        HTML("This tool allows you to search for files on the network directory using historically indexed file locations. <br>"),
                                        HTML("It is also considerably more effective at copying files in bulk using a key word (see the 'Actions' section). <br>"),
                                        HTML("The tool is also designed to help employees understand what type of files are where, using summaries and charts. <br>"),
                                        HTML("To update the index data, please run the associated batch & R files in the 'data-raw' folder of the source code. <br>")
                                        ),
                                    ),
                                    # shiny::tags$p(img(src='www/logo.png')),
                                    shiny::tags$p("Oil, Gas, and Alternative Energy Division"),
                                    shiny::tags$p("Developed by:",img(src='www/userimageC.png', align = "center")),
                                    shiny::tags$p("Zack Kedida <zackbk@gmail.com>"),
                                    close_button_label = "Click to Return!",
                                    display_button = TRUE)
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
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ShareDriveApp'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyalert::useShinyalert() 
  )
}

