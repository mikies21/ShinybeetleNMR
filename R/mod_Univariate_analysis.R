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
          column(
            width = 6,
            uiOutput(outputId = ns("dividend_UI"))
          ),
          column(
            width = 6,
            uiOutput(outputId = ns("divisor_UI"))
          )
        ),
        fluidRow(
          plotly::plotlyOutput(outputId = ns("vulcano_plot")) # , brush = ns("plot_brush"))
        )
      )
    ),
    fluidRow(
      shinydashboard::box(
        title = "boxplots of brushed points",
        width = 7,
        ## clear clicked points
        actionButton(ns("clear_selection"), label = "clear selection", width = "25%"),
        ### test brushed and clicked points
        
        DT::dataTableOutput(ns("test_brush"))
        
        #plotOutput(outputId = ns("boxplot_univ"))
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
      selectInput(inputId = ns("divisor"), label = "divisor", choices = divs, selected = divs[2], multiple = F)
    })

    FC_data <- reactive({
      FC_data <- NMRmetab_foldchange(data = data_NMR_n(), groupID = grouping_var(), index_col = index_metadata() + 1, dividendID = input$dividend, input$divisor)

      merged_data <- merge(x = FC_data, y = univ_test_data()$out[, c(1, 4)], by.x = "metabolite", by.y = "Metabolite/Bucket") %>%
        dplyr::mutate(dif_exps = "not significant") %>%
        dplyr::mutate(
          BH_pvals = -log10(`BH pvals`),
          dif_exps = dplyr::case_when(
            abs(log2FC) >= 0.6 & BH_pvals >= 1.30102999566 ~ "Significant & FoldChange ",
            abs(log2FC) < 0.6 & BH_pvals >= 1.30102999566 ~ "Significant",
            abs(log2FC) >= 0.6 & BH_pvals < 1.30102999566 ~ "FoldChange",
            TRUE ~ dif_exps
          ),
          delabel = dplyr::case_when(dif_exps != "not significant" ~ metabolite)
        )
      merged_data
    })

    hline <- function(y = 0, color = "black") {
      list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color, dash = "dot")
      )
    }
    vline <- function(x = 0, color = "black") {
      list(
        type = "line",
        y0 = 0,
        y1 = 1,
        yref = "paper",
        x0 = x,
        x1 = x,
        line = list(color = color, dash = "dot")
      )
    }
    
    
    ############# selecting data from plot
    
    
    volc_selects <- reactiveVal()
    
    observeEvent(plotly::event_data("plotly_click", source = "FC"), {
      volc_select <- plotly::event_data("plotly_click", source = "FC")$customdata
      volc_select_OLDNEW <- c(volc_selects(), volc_select)
      volc_selects(unique(volc_select_OLDNEW))
    })
    
    observeEvent(plotly::event_data("plotly_legendclick", source = "FC", priority = "event"), {
      volc_selects(NULL)
    })
    
    output$vulcano_plot <- plotly::renderPlotly({
      if (levels_group() < 3) {
        # click_data <- plotly::event_data("plotly_click",  priority = "event")
        # select_data <- plotly::event_data("plotly_selected", priority   = "event")
        df <- FC_data()
        
        cols <- ifelse(df$metabolite %in% volc_selects(), "red", "blue")
        
        p <- plotly::plot_ly(df,#key = rownames(df),
                             x = ~log2FC,
                             customdata = ~metabolite,source = "FC",
          y = ~BH_pvals,
          color = ~dif_exps,
          text = ~metabolite,
          type = "scatter",
          mode = "markers",
          hovertemplate = paste(
            "%{text}",
            "<br>BH p-value: %{y:.3f},",
            "<br>log2FC: %{x:.2f}"
          )
        ) %>% 
          plotly::layout(
            shapes = list(
              vline(0.6),
              vline(-0.6),
              hline(-log10(0.05))
            ),
            title = "Vulcano Plot",
            xaxis = list(title = "Fold Change"),
            yaxis = list(title = "-log10(adj p-value)")
          ) # %>%
        # highlight("plotly_selected", dynamic = TRUE)

        p
      } else {
        ### add plot for multivariate analysis
        NULL
      }
    })
    
    
    #v <- reactiveValues(data = plotly_data_sel)
    
    #observeEvent(input$clear_selection, {
    #  v$data() <- NULL
    #})

    output$univ_table <- DT::renderDataTable({
      DT::datatable(univ_test_data()$out,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          autoWidth = FALSE, scrollX = TRUE,
          columnDefs = list(list(
            width = "125px", targets = "_all"
          ))
        )
      )
    })

    
    ######### check brushed and clicked points
    
    output$test_brush <- DT::renderDataTable({
      df <- FC_data()
      df_filtered <- df %>% dplyr::filter(metabolite %in% volc_selects())
      DT::datatable(df_filtered)
    })
    
    boxplot_groups <- reactive({
      xmin_i <- input$plot_brush$xmin
      xmax_i <- input$plot_brush$xmax
      ymin_i <- input$plot_brush$ymin
      ymax_i <- input$plot_brush$ymax

      brushed_points <- FC_data() %>%
        dplyr::filter(
          dplyr::between(log2FC, xmin_i, xmax_i),
          dplyr::between(BH_pvals, ymin_i, ymax_i)
        ) %>%
        dplyr::pull("metabolite")
      brushed_points_and_group <- c(grouping_var(), brushed_points)
      prep_boxplots <- data_NMR_n() %>%
        dplyr::select(brushed_points_and_group) %>%
        tidyr::pivot_longer(cols = brushed_points, names_to = "metabolite", values_to = "value")
      prep_boxplots
    })

    output$boxplot_univ <- renderPlot({
      req(input$plot_brush$xmin)
      ggplot2::ggplot(boxplot_groups(), ggplot2::aes_string(x = grouping_var(), y = "value", fill = grouping_var())) +
        ggplot2::geom_boxplot() +
        ggplot2::geom_jitter(show.legend = F, width = 0.1) +
        ggplot2::facet_wrap(~metabolite, scales = "free_y") +
        ggplot2::theme_bw(base_size = 10)
    })
    ### VULCANO PLOT FOR comparison between 2 groups
  })
}



## To be copied in the UI
# mod_Univariate_analysis_ui("Univariate_analysis_ui_1")

## To be copied in the server
# mod_Univariate_analysis_server("Univariate_analysis_ui_1")
