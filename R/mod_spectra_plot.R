#' spectra_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spectra_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinydashboard::box(
      width = 12,
      shiny::radioButtons(inputId = ns("type_of_data"), label = "data to plot", choices = c("original", "normalised", "normalised and scaled"), selected = "normalised and scaled"),
      plotly::plotlyOutput(outputId = ns("plotspectra"))
    ),
    shinydashboard::box(
      width = 12,
      DT::dataTableOutput(outputId = ns("data_table"))
    )
  )
}

#' spectra_plot Server Functions
#'
#' @noRd
mod_spectra_plot_server <- function(id, data_NMR_original, data_NMR_n, data_NMR_ns, index_metadata, grouping_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    
    ### selecting colour pallette
    pal <- shiny::reactive({
      colourCount <- length(unique(data_NMR_n()[, grouping_var()]))
      if (colourCount > 8) {
        getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
        pal1 <- getPalette(colourCount)
      } else {
        pal1 <- RColorBrewer::brewer.pal(8, "Dark2")[1:colourCount]
      }
    })
    
    spectra_plot <- reactive({
      if (input$type_of_data == "original") {
        NMRMetab_plot_binned(data_NMR_original(), index_col = index_metadata() + 1, group_var = grouping_var())
      } else if (input$type_of_data == "normalised") {
        NMRMetab_plot_binned(data_NMR_n(), index_col = index_metadata() + 1, group_var = grouping_var())
      } else if (input$type_of_data == "normalised and scaled") {
        NMRMetab_plot_binned(data_NMR_ns(), index_col = index_metadata() + 1, group_var = grouping_var())
      }
    })
    output$plotspectra <- plotly::renderPlotly({
      p <- spectra_plot()$plot+
        ggplot2::scale_color_manual(values = pal())
      plotly::ggplotly(p, tooltip = "x") %>% 
        plotly::layout(hovermode = "x unified")
      
    })

    output$data_table <- DT::renderDataTable({
      spectra_plot()$sample_sum_df
    })
  })
}

## To be copied in the UI
# mod_spectra_plot_ui("spectra_plot_ui_1")

## To be copied in the server
# mod_spectra_plot_server("spectra_plot_ui_1")
