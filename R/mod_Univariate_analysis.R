#' Univariate_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Univariate_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' Univariate_analysis Server Functions
#'
#' @noRd 
mod_Univariate_analysis_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_Univariate_analysis_ui("Univariate_analysis_ui_1")
    
## To be copied in the server
# mod_Univariate_analysis_server("Univariate_analysis_ui_1")
