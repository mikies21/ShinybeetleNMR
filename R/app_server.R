#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  usethis::use_pipe()
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

  ## PLOT SPECTRAL BINS
  mod_spectra_plot_server("spectra_plot_ui_1",
    data_NMR_original = data_original,
    data_NMR_n = data_n,
    data_NMR_ns = data_ns,
    index_metadata = reactive({
      data_NMR$index_metadata()
    })
  )

  ## UNIVARIATE ANALYSIS
  mod_Univariate_analysis_server("Univariate_analysis_ui_1",
    data_NMR_n = data_n,
    index_metadata = reactive({
      data_NMR$index_metadata()
    })
  )
}
