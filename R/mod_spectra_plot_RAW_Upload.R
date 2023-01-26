#' spectra_plot_RAW_Upload UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_spectra_plot_RAW_Upload_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyWidgets::prettyRadioButtons(
        inputId = ns("selectInputData"),
        label = "Choose:",
        choices = c("JAKIRAW", "upload csv", "upload Bruker zip file"),
        icon = icon("check"),
        bigger = TRUE,
        status = "info",
        animation = "jelly"
      ),
    shiny::conditionalPanel(
      condition = "input.selectInputData == 'upload csv'",
      ns = ns,
      shiny::fileInput(inputId = ns("fileupload"), label = "upload csv file", multiple = F)
      ),
    shinyWidgets::prettyCheckbox(
      inputId = ns("PatternCheck"),
      label = "add Pattern file",
      value = F,
      status = "primary",
      shape = "round",
      animation = "jelly"
      ),
    shiny::conditionalPanel(
      condition = "input.PatternCheck == 1",
      ns = ns,
      shiny::fileInput(inputId = ns("PatternUpload"), label = "upload csv file", multiple = F)
      )
    )
}
    
#' spectra_plot_RAW_Upload Server Functions
#'
#' @noRd 
mod_spectra_plot_RAW_Upload_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
    selectionInput <- shiny::reactive({
      input$selectInputData
    })
    
    upfile <- shiny::reactive({
      if (selectionInput() == "JAKIRAW") {
        data("raw_data")
        return(raw_data)
      } else if (selectionInput() == "upload csv") {
        #return(NULL)
        file <- input$fileupload
        ext <- tools::file_ext(file$fileupload)
        
        req(file)
        
        return(read.csv(file$datapath))
      } else if (selectionInput() == "upload Bruker zip file") {
        return(NULL)
      }
      
      # data_CRS
    })
    
    pattern <- shiny::reactive({
      if (input$PatternCheck) {
        data("pattern_file")
        return(pattern_file)
      } else {
        return(NULL)
      }
    })
    
    return(
      list(
        "RAW_data_original" = reactive({
          upfile()
          }),
        "RAW_Pattern" = reactive({
          pattern()
        }), 
        "PatternCheck" = reactive({
          input$PatternCheck
        })
      )
    )
    
  })
}
    
## To be copied in the UI
# mod_spectra_plot_RAW_Upload_ui("spectra_plot_RAW_Upload_1")
    
## To be copied in the server
# mod_spectra_plot_RAW_Upload_server("spectra_plot_RAW_Upload_1")
