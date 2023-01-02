convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}
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
    
    bs4Dash::dashboardPage(
      title = "ShinybeetlesNMR",
      fullscreen = TRUE,
      header = bs4Dash::dashboardHeader(
        title = "ShinybeetlesNMR",
        skin = "light",
        status = "white",
        border = TRUE,
        sidebarIcon = icon("bars"),
        controlbarIcon = icon("th"),
        fixed = FALSE
        ),
      sidebar = bs4Dash::dashboardSidebar(
        skin = "light",
        status = "primary",
        elevation = 3,
        minified = TRUE,
        bs4Dash::sidebarMenu(
          id = "sidebar",
          bs4Dash::menuItem(
            text = "Start Here",
            tabName = "start",
            selected = T
          ),
          bs4Dash::menuItem(
            text = "Plot Spectra", 
            tabName = "PlotSpectra"
          ),
          bs4Dash::sidebarHeader(title = "Analysis"),
          bs4Dash::menuItem(
            text = "home",
            tabName = "home",
            icon = icon("dashboard")
            ),
          bs4Dash::menuItem(
            text = "Univariate",
            tabName = "Univariate"),
          bs4Dash::menuItem(
            text = "PCA",
            tabName = "PCA"),
          bs4Dash::menuItem(
            text = "PLS-DA",
            tabName = "PLSDA")
        )
      ),
      
      
      bs4Dash::dashboardControlbar(
        skin = "light",
        pinned = TRUE,
        overlay = FALSE,
        collapsed = F, 
        mod_Upload_data_ui("Upload_data_ui_1"),
        
        #          # selectInput(inputId = "addfiltermod", label = "filtering options", choices = c("a", "b"))
        #        )),
        shiny::uiOutput(outputId = "groupingvariable_global_UI"),
        shiny::checkboxInput(inputId = "Q_filter_data", value = F, label = "filter dataframe"),
        shiny::conditionalPanel(
          condition = "input.Q_filter_data == true",
          datamods::filter_data_ui(id = "filter", show_nrow = TRUE, max_height = NULL)
          )
        ),
      
      
      body = bs4Dash::dashboardBody(
        bs4Dash::tabItems(
          bs4Dash::tabItem(
            tabName = "start",
            "start page, to be finished"
            ),
          bs4Dash::tabItem(
            tabName = "PlotSpectra",
            mod_spectra_plot_RAW_ui("spectra_plot_RAW_1")
          ),
          bs4Dash::tabItem(
            tabName = "home",
            shinydashboard::infoBoxOutput(outputId = "datadesc", width = 6),
            shinydashboard::infoBoxOutput(outputId = "datatrans", width = 6),
            mod_spectra_plot_ui("spectra_plot_ui_1")
            ),
          bs4Dash::tabItem(
            tabName =  "Univariate",
            mod_Univariate_analysis_ui("Univariate_analysis_ui_1")
            ),
          bs4Dash::tabItem(
            tabName = "PCA",
            mod_PCA_ui("PCA_ui_1")
            ),
          bs4Dash::tabItem(
            tabName = "PLSDA",
            mod_PLSDA_ui("PLSDA_ui_1")
            )
          )
        ),
      footer = bs4Dash::dashboardFooter(
        left = "Thank you for trying this app!",
        right = tags$a(href="https://twitter.com/micfresneda", "@micfresneda"),
        fixed = FALSE
        )
      )
    ### title Page
    #    shiny::titlePanel(shiny::HTML("<center>ShinybeetlesNMR</center>")),
    #
    #    shiny::sidebarLayout(
    #      position = "left",
    #      sidebarPanel(
    #        width = 2,
    #        mod_Upload_data_ui("Upload_data_ui_1"),
    #        shiny::uiOutput(outputId = "groupingvariable_global_UI"),
    #        shiny::checkboxInput(inputId = "Q_filter_data", value = F, label = "filter dataframe"),
    #        conditionalPanel(
    #          condition = "input.Q_filter_data == true",
    #          datamods::filter_data_ui(id = "filter", show_nrow = TRUE, max_height = NULL)
    #          # selectInput(inputId = "addfiltermod", label = "filtering options", choices = c("a", "b"))
    #        )
    #      ),
    #      mainPanel(
    #        width = 10,
    #        shiny::tabsetPanel(
    #          type = "tabs",
    #          tabPanel(
    #            title = "home",
    #            shinydashboard::infoBoxOutput(outputId = "datadesc", width = 6),
    #            shinydashboard::infoBoxOutput(outputId = "datatrans", width = 6),
    #            mod_spectra_plot_ui("spectra_plot_ui_1")
    #          ),
    #          shiny::tabPanel(
    #            title = "Univariate",
    #            mod_Univariate_analysis_ui("Univariate_analysis_ui_1")
    #          ),
    #          shiny::tabPanel(
    #            title = "PCA",
    #            mod_PCA_ui("PCA_ui_1")
    #          ),
    #          shiny::tabPanel(
    #            title = "PLS-DA",
    #            mod_PLSDA_ui("PLSDA_ui_1")
    #          )
    #        )
    #      )
    #    )
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
    #shinyWidgets::setBackgroundColor(color = "ghostwhite"),
    #shinyWidgets::useShinydashboard()
  )
}
