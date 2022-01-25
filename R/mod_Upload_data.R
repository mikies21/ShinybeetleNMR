library(shiny)
library()
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
    shiny::fileInput(inputId = ns("fileupload"), label = "upload csv file", multiple = F),
    shiny::sliderInput(inputId = ns("metadata_colnames"), label = "metadata columns", min = 1, max = 10, value = 9, step = 1),
    fluidRow(
      column(
        width = 6,
        shiny::selectInput(inputId = ns("normalisation"), choices = c("None", "PQN", "Auto"), label = "Normalisation"),
      ),
      column(
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

    upfile <- reactive({
      file <- input$fileupload
      ext <- tools::file_ext(file$fileupload)

      req(file)
      # validate(need(ext == ".csv", "Please upload a csv file"))

      read.csv(file$datapath)
      # return(data_CRS)
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

## To be copied in the UI
# mod_Upload_data_ui("Upload_data_ui_1")

## To be copied in the server
# mod_Upload_data_server("Upload_data_ui_1")
