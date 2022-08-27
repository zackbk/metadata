#' 1_body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_body_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinymaterial::material_row(
      shinymaterial::material_column(
        width = 3,offset = 3,
        shinymaterial::material_modal(modal_id = "indexmodal",button_text = "Create .Bat File",button_icon = "file",
                                      mod_4_engine_ui("4_engine_ui_1")
        )),
      shinymaterial::material_column(
        width = 3,
        shinymaterial::material_modal(modal_id = "searchmodal",button_text = "Search",button_icon = "search",
                                      mod_2_search_ui("2_search_ui_1")
        )),
      shinymaterial::material_column(
        width = 3,
        shinymaterial::material_modal(modal_id = "filtermodal",button_text = "Filter",button_icon = "filter",
                                      mod_2_2_subctlr_ui("2_2_subctlr_ui_1")
        ))
    ),
    shinymaterial::material_card(title = tags$h3("Results"),
                                 mod_3_view_ui("3_view_ui_1")
    )
  )
}

#' 1_body Server Function
#'
#' @noRd 
mod_1_body_server <- function(input, output, session, r){
  ns <- session$ns
  print("RUN mod_1")
  
  callModule(mod_4_engine_server,"4_engine_ui_1", r)
  callModule(mod_2_search_server, "2_search_ui_1", r)
  callModule(mod_2_2_subctlr_server, "2_2_subctlr_ui_1", r)
  callModule(mod_3_view_server, "3_view_ui_1", r)
  
  
  print("End mod_1")
}

## To be copied in the UI
# 

## To be copied in the server
# 

