#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  ## To be copied in the server
  data_NMR <- mod_Upload_data_server("Upload_data_ui_1")
  
  
  output$datadesc <- shinydashboard::renderInfoBox({
    samples <- nrow(data_NMR$data_ns())
    metabolites <- ncol(data_NMR$data_ns())
    shinydashboard::infoBox(title = "DATA Description", color = "red", fill = T,
                            value = paste0("Data uploaded has ", samples, " and ", metabolites))
  })
  
  output$datatrans <- shinydashboard::renderInfoBox({
    
    shinydashboard::infoBox(title = "Normalisation and Scaling", color = "blue", fill = T,
                            value = paste0("Data has been normalised by ", data_NMR$normalisation(), " and scaled by ", data_NMR$scaling()))
  })
  
  output$grouping_variable_spectra_UI <- shiny::renderUI({
    shiny::selectInput(inputId = "grouping_variable_spectra", label = "choose grouping variable", choices = colnames(data_NMR$data_ns()[,1:data_NMR$index_metadata()]))
  })
  
  output$plotspectra <- shiny::renderPlot({
    if (input$type_of_data == "original") {
      NMRMetab_plot_binned(data_NMR$data_original(), index_col = data_NMR$index_metadata()+1, group_var = input$grouping_variable_spectra)
    } else if (input$type_of_data == "normalised") {
      NMRMetab_plot_binned(data_NMR$data_n(), index_col = data_NMR$index_metadata()+1, group_var = input$grouping_variable_spectra)
    } else if (input$type_of_data == "normalised and scaled") {
      NMRMetab_plot_binned(data_NMR$data_ns(), index_col = data_NMR$index_metadata()+1, group_var = input$grouping_variable_spectra)
    }
  })
  
}
