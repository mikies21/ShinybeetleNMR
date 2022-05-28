#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PCA_ui <- function(id) {
  ns <- NS(id)
    fluidPage(
      fluidRow(
      column(
        width = 1,
        numericInput(inputId = ns("PCx"), label = "PCx", value = 1, min = 1, max = 15, step = 1),
        shinyWidgets::materialSwitch(
          inputId = ns("elipses"),
          label = "add ellipses",
          value = FALSE,
          status = "primary"
        )
      ),
      column(
        width = 1,
        numericInput(inputId = ns("PCy"), label = "PCy", value = 2, min = 1, max = 15, step = 1)
      ),
      shinydashboard::box(
        width = 5,
        plotly::plotlyOutput(outputId = ns("PCA_plot"))
      ),
      shinydashboard::box(
        width = 5,
        shiny::plotOutput(outputId = ns("PCA_loading_plot"))
      )
    ),
    fluidRow(
      DT::dataTableOutput(outputId = ns("table_PC"), width = 12)
    ))
}

#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, data_NMR_ns, index_metadata, grouping_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    PCA_res <- reactive({
      PCA <- prcomp(x = data_NMR_ns()[, c(index_metadata() + 1):ncol(data_NMR_ns())], center = F, scale. = F)
    })

    output$PCA_plot <- plotly::renderPlotly({
      prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)

      plot1 <- ggplot2::ggplot(as.data.frame(PCA_res()$x), aes_string(x = paste0("PC", input$PCx), y = paste0("PC", input$PCy))) +
        ggplot2::geom_point(aes(colour = data_NMR_ns()[, grouping_var()])) +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::labs(
          x = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = ""),
          y = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")
        ) +
        ggplot2::guides(colour = guide_legend(title = grouping_var()))

      if (isTRUE(input$elipses)) {
        plot1 <- plot1 + ggplot2::stat_ellipse(aes(colour = data_NMR_ns()[, grouping_var()]), show.legend = F)
      }
      plotly::ggplotly(plot1, source = "pointsOfInterest")
    })


    output$PCA_loading_plot <- renderPlot({
      prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)
      plot2 <- ggplot2::ggplot(
        as.data.frame(PCA_res()$rotation),
        aes_string(x = paste0("PC", input$PCx), y = paste0("PC", input$PCy))
      ) +
        ggplot2::geom_point() +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::labs(
          title = "PCA loadings",
          x = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = ""),
          y = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")
        )
      plot2
    })
    
    
    output$table_PC <- DT::renderDataTable({
      d <- plotly::event_data("plotly_click", source = "pointsOfInterest")
    })
    
  })
}

## To be copied in the UI
# mod_PCA_ui("PCA_ui_1")

## To be copied in the server
# mod_PCA_server("PCA_ui_1")
