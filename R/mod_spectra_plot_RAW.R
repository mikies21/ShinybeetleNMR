#' spectra_plot_RAW UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spectra_plot_RAW_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shinyWidgets::prettyRadioButtons(
          inputId = ns("selectInputData"),
          label = "Choose:",
          choices = c("JAKIRAW", "upload csv", "upload Bruker zip file"),
          icon = icon("check"),
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        )
      ),
      shiny::column(
        width = 6,
        shiny::conditionalPanel(
          condition = "input.selectInputData == 'upload csv'",
          ns = ns,
          shiny::fileInput(inputId = ns("fileupload"), label = "upload csv file", multiple = F)
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shinyWidgets::prettyCheckbox(
          inputId = ns("PatternCheck"),
          label = "add Pattern file",
          value = F,
          status = "primary",
          shape = "round",
          animation = "jelly"
        )
      ),
      shiny::column(
        width = 6,
        shiny::conditionalPanel(
          condition = "input.PatternCheck == 1",
          ns = ns,
          shiny::fileInput(inputId = ns("PatternUpload"), label = "upload csv file", multiple = F)
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::actionButton(
          inputId = ns("add_graph"),
          label = "Add Graph"
        )
      )
    ),
    shiny::fluidRow(
      column(
        width = 12,
        shinyWidgets::sliderTextInput(
          inputId = ns("RangePpm"),
          width = "100%",
          label = "range to plot",
          choices = seq(from = 10, to = 0, by = -0.1),
          selected = c(2.1, 2),
          grid = T
        )
      )
    ),
    shiny::column(
      width = 12,
      shiny::plotOutput(outputId = ns("PlotSpetra1")),
      shiny::actionButton(
        inputId = ns("BinRawData"),
        label = "Bin the data"
      ),
      shiny::downloadButton("downloadBinnedData", "Download"), 
      DT::dataTableOutput(outputId = ns("BinnedData"))
    )
  )
}

#' spectra_plot_RAW Server Functions
#'
#' @noRd
mod_spectra_plot_RAW_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selectionInput <- shiny::reactive({
      input$selectInputData
    })

    upfile <- shiny::reactive({
      if (selectionInput() == "JAKIRAW") {
        data("raw_data")
        return(raw_data)
      } else if (selectionInput() == "upload csv") {
        return(NULL)
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

    # output$test1 <- DT::renderDataTable({
    #  data("raw_data")
    #  return(raw_data)
    #
    # })
    filter_raw <- eventReactive(input$add_graph, {
      upfile()[upfile()$ppm >= input$RangePpm[2] & upfile()$ppm <= input$RangePpm[1], ]
    })
    
    filter_pattern <- eventReactive(input$add_graph, {
      if (!input$PatternCheck) {
        return(NULL)
      } else {
        pattern()[pattern()$min_ppm >= input$RangePpm[2] & pattern()$max_ppm <= input$RangePpm[1], ]
      }
    })

    filter_raw_melted <- reactive({
      tidyr::pivot_longer(
        data = filter_raw(),
        cols = !1, names_to = "sampleID",
        values_to = "value"
      )
    })

    spectra_bin_plot <- eventReactive(input$add_graph, {
      MaxPoint <- max(filter_raw_melted()$value)
      
      p <- ggplot2::ggplot() +
        ggplot2::geom_line(
          data = filter_raw_melted(),
          ggplot2::aes(
            x = ppm,
            y = value,
            colour = sampleID
          ),
          show.legend = F
        ) 
      
      if (input$PatternCheck) {
        p <- p +
          ggplot2::geom_rect(
            data = filter_pattern(),
            ggplot2::aes(
              xmin = min_ppm, 
              xmax = max_ppm, 
              ymax = Inf,
              ymin = -Inf), 
            alpha = 0.2, 
            fill = "grey", 
            colour = 'black')+
          ggplot2::geom_text(
            data = filter_pattern(),
            ggplot2::aes(
              x = (min_ppm+max_ppm)/2,
              y = MaxPoint,
              label = bin
            ), 
            angle = 45,
            hjust = 0.1, 
            size = 3
          )
      }
      p +
        ggplot2::scale_x_reverse()+
        ggplot2::theme_light()
    })

    output$PlotSpetra1 <- shiny::renderPlot({
      spectra_bin_plot()
    })
    
    #########################################
    binned_df <- eventReactive(input$BinRawData, {
      list_of_bins = list()
      
      for (i in 1:nrow(pattern())) {
        df = upfile()[upfile()$ppm >= pattern()[i, "min_ppm"] & upfile()$ppm <= pattern()[i, "max_ppm"], ] 
      
        ###### print error  
       # if (nrow(df) == 0) {
      #  print(paste0('0 rows ---- ',pattern_file[i,'bin'],'  min=', pattern_file[i, 'min_ppm'],'  max=',pattern_file[i, 'max_ppm']))
       # }
        ###################
        list_of_bins[[i]] = df
        names(list_of_bins)[[i]] = pattern()[i,'bin']
      }
      
      df = list_of_bins %>% lapply(function(x){
        ppm = x$ppm
        #print(ppm)
        apply(x %>% dplyr::select(-ppm), 2, function(y) {
          areaucurve = bayestestR::area_under_curve(x = ppm, y = y, method = 'trapezoid') * 1000
        })
      }) %>%
        dplyr::bind_rows(.id = 'bin') %>%
        #dplyr::select(-ppm) %>%
        t() %>%
        as.data.frame() %>%
        janitor::row_to_names(1) %>%
        tibble::rownames_to_column('sampleID') %>%
        tibble::tibble()
      
      df[2:ncol(df)] = apply(df[2:ncol(df)], 2 , as.numeric)
      
      return(df)
    })
    
    
    output$BinnedData <- DT::renderDataTable({
      binned_df()[1:3, 1:10]
    })
    
    output$downloadBinnedData <- shiny::downloadHandler(
      filename = function() {
        "BinnedData.csv"
      },
      content = function(file) {
        write.csv(x = binned_df(), file, row.names = FALSE)
      }
    )
    
    
    #########################################
  })
}

## To be copied in the UI
# mod_spectra_plot_RAW_ui("spectra_plot_RAW_1")

## To be copied in the server
# mod_spectra_plot_RAW_server("spectra_plot_RAW_1")
