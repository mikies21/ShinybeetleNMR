#' Upload_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Upload_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shinyWidgets::prettyRadioButtons(
          inputId = ns("selectInputData"),
          label = "Choose:",
          choices = c("JAKi data (example)", "upload csv"),
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly")
        )
      ),
      fluidRow(
        shiny::column(
          width = 12,
          shiny::conditionalPanel(
            condition = "input.selectInputData == 'upload csv'",
            ns = ns,
            shiny::fileInput(inputId = ns("fileupload"), label = "upload csv file", multiple = F)
            )
          )
        ),    
          # selectInput(inputId = "addfiltermod", label = "filtering options", choices = c("a", "b")),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::sliderInput(inputId = ns("metadata_colnames"), label = "metadata columns", min = 1, max = 10, value = 9, step = 1)
      )
    ),
    #shiny::actionButton(inputId = ns("start_everything"), label = "start analysis", width = "100%"), 
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::selectInput(inputId = ns("normalisation"), choices = c("None", "PQN", "Auto"), label = "Normalisation"),
      ),
      shiny::column(
        width = 6,
        shiny::selectInput(inputId = ns("scaling"), choices = c("None", "Pareto", "Auto"), label = "Scaling")
      )
    )
    # datamods::filter_data_ui(id = ns("filter"), show_nrow = TRUE, max_height = NULL)
  )
}




#' Upload_data Server Functions
#'
#' @noRd
mod_Upload_data_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selectionInput <- reactive({
      input$selectInputData
    })
    
    upfile <- reactive({

      if (selectionInput() == "JAKi data (example)") {
        data("data_CRS")
        return(data_CRS)
      } else {
        file <- input$fileupload
        ext <- tools::file_ext(file$fileupload)
        
        req(file)
        
        return(read.csv(file$datapath))
      }
      
      # data_CRS
    })

    data_n <- reactive({
      NMRMetab_norm_scale(upfile(), index_col = input$metadata_colnames + 1, normalisation = input$normalisation, bin = NA, scaling = "None", writeToFile = F)
    })


    # res_filter <- datamods::filter_data_server(
    #  id = "filter",
    #  data = data_norm,
    #  vars = reactive(as.list(colnames(data_norm()[2:input$metadata_colnames]))),
    #  name = reactive("data_nmr"),
    #  drop_ids = TRUE,
    #  widget_char = c("select"),
    #  widget_num = c("slider"),
    #  widget_date = c("slider"),
    #  label_na = "NA"
    # )

    # data_n <- shiny::reactive({
    #  res_filter$filtered()
    # })

    data_ns <- reactive({
      NMRMetab_norm_scale(data_n(), index_col = input$metadata_colnames + 1, normalisation = "None", bin = NA, scaling = input$scaling, writeToFile = F)
    })


    return(
      list(
        "data_original" = reactive({
          upfile()
        }),
        "data_n" = reactive({
          data_n()
        }),
        "data_ns" = reactive({
          data_ns()
        }),
        "index_metadata" = reactive({
          input$metadata_colnames
        }),
        "normalisation" = reactive({
          input$normalisation
        }),
        "scaling" = reactive({
          input$scaling
        })
      )
    )
  })
}
