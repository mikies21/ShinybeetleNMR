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
    shiny::navbarPage(title = h1("ShinybeetleNMR"),
                      shiny::tabPanel(title = 'home',
                                      shiny::sidebarLayout(
                                        shiny::sidebarPanel(
                                          mod_Upload_data_ui("Upload_data_ui_1")
                                        ),
                                        shiny::mainPanel(
                                          shinydashboard::infoBoxOutput(outputId = "datadesc", width = 6),
                                          shinydashboard::infoBoxOutput(outputId = "datatrans", width = 6),
                                          shinydashboard::box(width = 12,
                                            shiny::uiOutput(outputId = "grouping_variable_spectra_UI"),
                                            shiny::radioButtons(inputId = "type_of_data", label = "data to plot", choices = c("original", "normalised", "normalised and scaled")),
                                            shiny::plotOutput(outputId = "plotspectra")
                                          )
                                          )
                                        )
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
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'ShinybeetleNMR'
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
    shinyWidgets::setBackgroundColor(color = "ghostwhite"),
    shinyWidgets::useShinydashboard()
  )
}

