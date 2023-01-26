#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  
  #### ui outputs for " Display controlbarMenu() in right sidebar of shiny dashboard based on tab selection"
  output$AnalysisInputs <- renderUI({
    tagList(
      fluidPage(
      conditionalPanel(condition = 'input.sidebar=="home" || input.sidebar=="Univariate" || input.sidebar=="PCA" || input.sidebar=="PLSDA"' ,
                       mod_Upload_data_ui("Upload_data_ui_1"),
                       
                                 # selectInput(inputId = "addfiltermod", label = "filtering options", choices = c("a", "b"))
                       #        )),
                       shiny::uiOutput(outputId = "groupingvariable_global_UI"),
                       shiny::checkboxInput(inputId = "Q_filter_data", value = F, label = "filter dataframe"),
                       shiny::conditionalPanel(
                         condition = "input.Q_filter_data == true",
                         datamods::filter_data_ui(id = "filter", show_nrow = TRUE, max_height = NULL)
                         )
                       ), 
      conditionalPanel(condition = 'input.sidebar=="PlotSpectra"', 
                       mod_spectra_plot_RAW_Upload_ui("spectra_plot_RAW_Upload_1")
                       )
      )
    )
  })
  
  
  ######### Raw spectra server
  RAW_data <- mod_spectra_plot_RAW_Upload_server("spectra_plot_RAW_Upload_1")
  
  ########## Raw spectra server
  mod_spectra_plot_RAW_server("spectra_plot_RAW_1",
                              upfile = reactive({
                                RAW_data$RAW_data_original()
                              }),
                              pattern = reactive({
                                RAW_data$RAW_Pattern()
                              }),
                              PatternCheck = reactive({
                                RAW_data$PatternCheck()
                              })
                              )
  
  ## Upload the data

  data_NMR <- mod_Upload_data_server("Upload_data_ui_1")
  # get the metadata columns (ALL DATA)
  metadata_columns <- reactive({
    as.list(colnames(data_NMR$data_original()[1:data_NMR$index_metadata()]))
  })

  if (isFALSE(reactive({
    input$Q_filter_data
  }))) {
    n_metabolites <- reactive({
      ncol(data_NMR$data_original())
    })
    n_samples <- reactive({
      nrow(data_NMR$data_original())
    })
    data_original <- reactive({
      data_NMR$data_original()
    })
    data_n <- reactive({
      data_NMR$data_n()
    })
    data_ns <- reactive({
      data_NMR$data_ns()
    })
  } else {
    res_filter <- datamods::filter_data_server(
      id = "filter",
      data = data_NMR$data_n,
      vars = reactive(metadata_columns()),
      name = reactive("data_nmr"),
      drop_ids = TRUE,
      widget_char = c("select"),
      widget_num = c("slider"),
      widget_date = c("slider"),
      label_na = "NA"
    )
    data_original <- reactive({
      data_NMR$data_original()
    })
    n_metabolites <- reactive({
      ncol(res_filter$filtered())
    })
    n_samples <- reactive({
      nrow(res_filter$filtered())
    })
    data_n <- reactive({
      res_filter$filtered()
    })
    data_ns <- reactive({
      NMRMetab_norm_scale(data = res_filter$filtered(), index_col = data_NMR$index_metadata() + 1, scaling = data_NMR$scaling())
    })
  }

  output$datadesc <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = "DATA Description", color = "red", fill = T, icon = icon("chart-bar"),
      value = paste0("Data uploaded has ", n_samples(), "samples and ", n_metabolites(), " metabolites")
    )
  })

  output$datatrans <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      title = "Normalisation and Scaling", color = "blue", fill = T, icon = icon("chart-bar"),
      value = paste0("Data has been normalised by ", data_NMR$normalisation(), " and scaled by ", data_NMR$scaling())
    )
  })

  output$groupingvariable_global_UI <- shiny::renderUI({
    shiny::selectInput(inputId = "groupingvariable_global", label = "choose grouping variable global", choices = colnames(data_original()[, 1:data_NMR$index_metadata()]))
  })

  ## PLOT SPECTRAL BINS
  mod_spectra_plot_server("spectra_plot_ui_1",
    data_NMR_original = data_original,
    data_NMR_n = data_n,
    data_NMR_ns = data_ns,
    index_metadata = reactive({
      data_NMR$index_metadata()
    }),
    grouping_var = reactive({
      input$groupingvariable_global
    })
  )

  ## UNIVARIATE ANALYSIS
  mod_Univariate_analysis_server("Univariate_analysis_ui_1",
    data_NMR_n = data_n,
    index_metadata = reactive({
      data_NMR$index_metadata()
    }),
    grouping_var = reactive({
      input$groupingvariable_global
    })
  )



  mod_PCA_server("PCA_ui_1",
    data_NMR_ns = data_ns,
    index_metadata = reactive({
      data_NMR$index_metadata()
    }),
    grouping_var = reactive({
      input$groupingvariable_global
    })
  )



  mod_PLSDA_server("PLSDA_ui_1",
    data_NMR_ns = data_ns,
    index_metadata = reactive({
      data_NMR$index_metadata()
    }),
    grouping_var = reactive({
      input$groupingvariable_global
    })
  )
}
