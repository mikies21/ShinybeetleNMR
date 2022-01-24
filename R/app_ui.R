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
    shiny::sidebarLayout(
      sidebarPanel(width = 2,
        mod_Upload_data_ui("Upload_data_ui_1"),
        shiny::checkboxInput(inputId = "Q_filter_data", value = F, label = "filter dataframe"),
        conditionalPanel(
          condition = "input.Q_filter_data == true",
          datamods::filter_data_ui(id = "filter", show_nrow = TRUE, max_height = NULL)
          # selectInput(inputId = "addfiltermod", label = "filtering options", choices = c("a", "b"))
        )
      ),
      mainPanel(
        navbarPage(
          title = "ShinybeetlesNMR",
          shiny::tabPanel(
            title = "home",
            shinydashboard::infoBoxOutput(outputId = "datadesc", width = 6),
            shinydashboard::infoBoxOutput(outputId = "datatrans", width = 6),
            mod_spectra_plot_ui("spectra_plot_ui_1")
          ),
          shiny::tabPanel(title = "Univariate"),
          shiny::tabPanel(title = "PCA"),
          shiny::tabPanel(title = "PLS-DA")
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
golem_add_external_resources <- function() {
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ShinybeetleNMR"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyWidgets::setBackgroundColor(color = "ghostwhite"),
    shinyWidgets::useShinydashboard()
  )
}
