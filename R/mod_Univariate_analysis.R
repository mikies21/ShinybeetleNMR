#' Univariate_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Univariate_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      shinydashboard::box(
        width = 6,
        fluidRow(
          column(
            width = 12,
            shiny::uiOutput(outputId = ns("grouping_variable_spectra_UI")),
            shiny::uiOutput(outputId = ns("args_univ"))
            )
        )
      ),
      shinydashboard::infoBoxOutput(outputId = ns("univ_information"), width = 6)
    ),
    fluidRow(
      shinydashboard::box(title = "table univariate",
                          width = 6,
                          DT::dataTableOutput(outputId = ns("univ_table"))),
      shinydashboard::box(title = "vulcano plot",
                          width = 6
        
      )
    )
  )
}

#' Univariate_analysis Server Functions
#'
#' @noRd
mod_Univariate_analysis_server <- function(id, data_NMR_n, index_metadata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$grouping_variable_spectra_UI <- shiny::renderUI({
      shiny::selectInput(inputId = ns("grouping_variable_spectra"), label = "choose grouping variable", choices = colnames(data_NMR_n()[, 1:index_metadata()]))
    })
    
    levels_group <- reactive({
      length(unique(data_NMR_n()[,input$grouping_variable_spectra]))
    })
    
    univ_data_text <- reactive({
      if (levels_group() < 3) {
        type_of_univ <- "something like T-test"
      } else {
        type_of_univ <- "ANOVA"
      }
    })
    
    
    output$args_univ <- renderUI({
      if (levels_group() < 3) {
        shinyWidgets::awesomeCheckboxGroup(
          inputId = ns("univ_ttest"),
          label = "univariate test", 
          choices = c("Paired", "Normal", "Equal variance"),
          inline = TRUE, 
          status = "danger"
        )
      } else {
        shiny::selectInput(inputId = ns('anova_test'),label = 'type of correction', 
                           choices = c('fdr', 'BH'))
      }
      
    })
    
    

    
    univ_test_data <- reactive({
      if (levels_group() < 3) {
        if (is.element('Paired',input$univ_ttest) & is.element('Normal',input$univ_ttest) & is.element('Equal variance',input$univ_ttest)) {
        res <- NMRMetab_UnivarTest(data = data_NMR_n(), index_col = index_metadata()+1, 
                                   group = input$grouping_variable_spectra, 
                                   paired = T, 
                                   normality = T, 
                                   equal.variance = T)
        } else if (is.element('Paired',input$univ_ttest) & is.element('Normal',input$univ_ttest) & !is.element('Equal variance',input$univ_ttest)) {
            res <- NMRMetab_UnivarTest(data = data_NMR_n(), index_col = index_metadata()+1, 
                                                 group = input$grouping_variable_spectra, 
                                                 paired = T, 
                                                 normality = T, 
                                                 equal.variance = F)
        } else if (is.element('Paired',input$univ_ttest) & !is.element('Normal',input$univ_ttest) & !is.element('Equal variance',input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(data = data_NMR_n(), index_col = index_metadata()+1, 
                                     group = input$grouping_variable_spectra, 
                                     paired = T, 
                                     normality = F, 
                                     equal.variance = F)
        } else if (!is.element('Paired',input$univ_ttest) & !is.element('Normal',input$univ_ttest) & !is.element('Equal variance',input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(data = data_NMR_n(), index_col = index_metadata()+1, 
                                     group = input$grouping_variable_spectra, 
                                     paired = F, 
                                     normality = F, 
                                     equal.variance = F)
        } else if (!is.element('Paired',input$univ_ttest) & is.element('Normal',input$univ_ttest) & !is.element('Equal variance',input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(data = data_NMR_n(), index_col = index_metadata()+1, 
                                     group = input$grouping_variable_spectra, 
                                     paired = F, 
                                     normality = T, 
                                     equal.variance = F)
        } else if (!is.element('Paired',input$univ_ttest) & !is.element('Normal',input$univ_ttest) & is.element('Equal variance',input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(data = data_NMR_n(), index_col = index_metadata()+1, 
                                     group = input$grouping_variable_spectra, 
                                     paired = F, 
                                     normality = F, 
                                     equal.variance = T)
        } else if (!is.element('Paired',input$univ_ttest) & is.element('Normal',input$univ_ttest) & is.element('Equal variance',input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(data = data_NMR_n(), index_col = index_metadata()+1, 
                                     group = input$grouping_variable_spectra, 
                                     paired = F, 
                                     normality = T, 
                                     equal.variance = T)
        }
        
          
      } else {
          res <- list(
            'out' = NMRMetab_anova(data = data_NMR_n(), group = input$grouping_variable_spectra, sigLevel = 0.05, adjMethod = 'fdr', index_col = index_metadata()+1)$anova_pvals,
            'test' = 'ANOVA') 
        
        }
    })
    
    output$univ_information <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(title = 'Type of Univariate',
                              icon = icon("chart-bar"),
                              value = HTML(paste0("number of levels: ", levels_group(), br(),
                                             univ_test_data()$test))
      )
    })
    
    output$univ_table <- DT::renderDataTable({
      univ_test_data()$out
    })
    
    ### VULCANO PLOT FOR comparison between 2 groups
    
    
  })
}

## To be copied in the UI
# mod_Univariate_analysis_ui("Univariate_analysis_ui_1")

## To be copied in the server
# mod_Univariate_analysis_server("Univariate_analysis_ui_1")
