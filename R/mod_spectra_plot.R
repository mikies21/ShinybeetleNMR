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
      shiny::uiOutput(outputId = ns("grouping_variable_spectra_UI")),
      shiny::radioButtons(inputId = ns("type_of_data"), label = "data to plot", choices = c("original", "normalised", "normalised and scaled")),
      shiny::plotOutput(outputId = ns("plotspectra"))
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
mod_spectra_plot_server <- function(id, data_NMR_original, data_NMR_n, data_NMR_ns, index_metadata) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$grouping_variable_spectra_UI <- shiny::renderUI({
      shiny::selectInput(inputId = ns("grouping_variable_spectra"), label = "choose grouping variable", choices = colnames(data_NMR_original()[, 1:index_metadata()]))
    })

    spectra_plot <- reactive({
      if (input$type_of_data == "original") {
        NMRMetab_plot_binned(data_NMR_original(), index_col = index_metadata() + 1, group_var = input$grouping_variable_spectra)
      } else if (input$type_of_data == "normalised") {
        NMRMetab_plot_binned(data_NMR_n(), index_col = index_metadata() + 1, group_var = input$grouping_variable_spectra)
      } else if (input$type_of_data == "normalised and scaled") {
        NMRMetab_plot_binned(data_NMR_ns(), index_col = index_metadata() + 1, group_var = input$grouping_variable_spectra)
      }
    })
    output$plotspectra <- shiny::renderPlot({
      plot(spectra_plot())
    })

    output$data_table <- DT::renderDataTable({
      ggplot2::ggplot_build(spectra_plot())$plot$data
    })
  })
}

## To be copied in the UI
# mod_spectra_plot_ui("spectra_plot_ui_1")

## To be copied in the server
# mod_spectra_plot_server("spectra_plot_ui_1")
