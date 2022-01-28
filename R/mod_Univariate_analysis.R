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
            shiny::uiOutput(outputId = ns("args_univ"))
          )
        )
      ),
      shinydashboard::infoBoxOutput(outputId = ns("univ_information"), width = 6)
    ),
    fluidRow(
      shinydashboard::box(
        title = "table univariate",
        width = 6,
        DT::dataTableOutput(outputId = ns("univ_table"))
      ),
      shinydashboard::box(
        title = "vulcano plot",
        width = 6,
        fluidRow(
          column(width = 6,
                 uiOutput(outputId = ns("dividend_UI"))
                 ),
          column(width = 6,
                 uiOutput(outputId = ns("divisor_UI"))
          )
        ),
        fluidRow(
          shiny::plotOutput(outputId = ns("vulcano_plot"))
        )
      )
    )
  )
}

#' Univariate_analysis Server Functions
#'
#' @noRd
mod_Univariate_analysis_server <- function(id, data_NMR_n, index_metadata, grouping_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    levels_group <- reactive({
      length(unique(data_NMR_n()[, grouping_var()]))
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
        shiny::selectInput(
          inputId = ns("anova_test"), label = "type of correction",
          choices = c("fdr", "BH")
        )
      }
    })




    univ_test_data <- reactive({
      if (levels_group() < 3) {
        if (is.element("Paired", input$univ_ttest) & is.element("Normal", input$univ_ttest) & is.element("Equal variance", input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(
            data = data_NMR_n(), index_col = index_metadata() + 1,
            group = grouping_var(),
            paired = T,
            normality = T,
            equal.variance = T
          )
        } else if (is.element("Paired", input$univ_ttest) & is.element("Normal", input$univ_ttest) & !is.element("Equal variance", input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(
            data = data_NMR_n(), index_col = index_metadata() + 1,
            group = grouping_var(),
            paired = T,
            normality = T,
            equal.variance = F
          )
        } else if (is.element("Paired", input$univ_ttest) & !is.element("Normal", input$univ_ttest) & !is.element("Equal variance", input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(
            data = data_NMR_n(), index_col = index_metadata() + 1,
            group = grouping_var(),
            paired = T,
            normality = F,
            equal.variance = F
          )
        } else if (!is.element("Paired", input$univ_ttest) & !is.element("Normal", input$univ_ttest) & !is.element("Equal variance", input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(
            data = data_NMR_n(), index_col = index_metadata() + 1,
            group = grouping_var(),
            paired = F,
            normality = F,
            equal.variance = F
          )
        } else if (!is.element("Paired", input$univ_ttest) & is.element("Normal", input$univ_ttest) & !is.element("Equal variance", input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(
            data = data_NMR_n(), index_col = index_metadata() + 1,
            group = grouping_var(),
            paired = F,
            normality = T,
            equal.variance = F
          )
        } else if (!is.element("Paired", input$univ_ttest) & !is.element("Normal", input$univ_ttest) & is.element("Equal variance", input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(
            data = data_NMR_n(), index_col = index_metadata() + 1,
            group = grouping_var(),
            paired = F,
            normality = F,
            equal.variance = T
          )
        } else if (!is.element("Paired", input$univ_ttest) & is.element("Normal", input$univ_ttest) & is.element("Equal variance", input$univ_ttest)) {
          res <- NMRMetab_UnivarTest(
            data = data_NMR_n(), index_col = index_metadata() + 1,
            group = grouping_var(),
            paired = F,
            normality = T,
            equal.variance = T
          )
        }
      } else {
        res <- list(
          "out" = NMRMetab_anova(data = data_NMR_n(), group = grouping_var(), sigLevel = 0.05, adjMethod = "fdr", index_col = index_metadata() + 1)$anova_pvals,
          "test" = "ANOVA"
        )
      }
    })

    output$univ_information <- shinydashboard::renderInfoBox({
      shinydashboard::infoBox(
        title = "Type of Univariate",
        icon = icon("chart-bar"),
        value = HTML(paste0(
          "number of levels: ", levels_group(), br(),
          univ_test_data()$test
        ))
      )
    })
    
    
    ### VULCANO PLOT
    output$dividend_UI <- renderUI({
      divs <- unique(data_NMR_n()[, grouping_var()])
      selectInput(inputId = ns("dividend"), label = "dividend", choices = divs, selected = divs[1], multiple = F)
    })
    
    
    output$divisor_UI <- renderUI({
      divs <- unique(data_NMR_n()[, grouping_var()])
      selectInput(inputId = ns("divisor"), label = "divisor", choices = divs, selected = divs[2],multiple = F)
    })
    
    
    output$vulcano_plot <- renderPlot({
      if (levels_group() < 3) {
        FC_data <- NMRmetab_foldchange(data = data_NMR_n(), groupID = grouping_var(), index_col = index_metadata() + 1, dividendID = input$dividend, input$divisor)
        
        merged_data <- merge(x = FC_data, y = univ_test_data()$out[,c(1,4)], by.x = "metabolite", by.y = "Metabolite/Bucket") %>%
          dplyr::mutate(dif_exps = 'NO') %>% 
          dplyr::mutate(BH_pvals = log10(`BH pvals`),
                 dif_exps = dplyr::case_when(log2FC > 0.6 & BH_pvals < 0.05 ~'UP',
                                      log2FC < c(-0.6) & BH_pvals < 0.05 ~'DOWN',
                                      TRUE~dif_exps),
                 delabel = dplyr::case_when(dif_exps != 'NO' ~ metabolite))
        
        ggplot2::ggplot(merged_data, ggplot2::aes(x = log2FC, y = -BH_pvals, label = delabel, fill = log2FC))+
          ggplot2::geom_point(colour = 'black',shape = 21, size = 3)+
          ggplot2::labs(y = '-Log 10 (adj p-value)', x = 'Log 2 (Fold Change)') +
          ggplot2::geom_vline(xintercept=c(-0.6, 0.6), col="black", linetype = 'dotted', size = 1) +
          ggplot2::geom_hline(yintercept=-log10(0.05), col="black", linetype = 'dotted', size = 1) +
          ggplot2::scale_fill_gradient2(low = "#5BAEF7", high = "#CA3617", mid = '#000000', )+
          #scale_fill_manual(values = c('#5BAEF7', '#000000', '#CA3617'))+
          #scale_color_brewer(palette = "Set1",)+
          ggplot2::theme_bw()+
          ggplot2::theme(legend.position = 'right')+
          ggrepel::geom_text_repel(show.legend = F, size = 3)
        
      } else {
        NULL
      }
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
