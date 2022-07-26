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
    shinymaterial::material_page(
      title = "ShinyDriveApp",
      primary_theme_color = "#3700B3",
      secondary_theme_color = "#3700B3",
      background_color = "#FFFFFF",
      font_color = "#121212",
      nav_bar_color = "#aeea00",
      # mod_1_body_ui("1_body_ui_1")         # Note: no comma
      shinymaterial::material_row(
        shinymaterial::material_column(width = 6,
                                       shinymaterial::material_card(title = tags$p("Index"),
                                                                    mod_4_engine_ui("4_engine_ui_1")
                                       )
        ),
        shinymaterial::material_column(width = 6,
                                       shinymaterial::material_card(title = tags$p("Search"),
                                                                    mod_2_controller_ui("2_controller_ui_1"),
                                       ),
                                       shinymaterial::material_card(title = tags$p("Filter"),
                                                                    mod_2_2_subctlr_ui("2_2_subctlr_ui_1")
                                       )
        )
      ),
      shinymaterial::material_card(title = tags$h3("Results"),
      mod_3_view_ui("3_view_ui_1")
      )
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
      app_title = 'ShareDriveApp'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyalert::useShinyalert() 
  )
}

