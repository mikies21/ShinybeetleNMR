#' PLSDA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PLSDA_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 1,
        numericInput(inputId = ns("PLSx"), label = "compx", value = 1, min = 1, max = 15, step = 1),
        shinyWidgets::materialSwitch(
          inputId = ns("elipses"),
          label = "add ellipses",
          value = FALSE,
          status = "primary"
        )
      ),
      column(
        width = 1,
        numericInput(inputId = ns("PLSy"), label = "compy", value = 2, min = 1, max = 15, step = 1)
      ),
      shinydashboard::box(
        width = 5,
        plotly::plotlyOutput(outputId = ns("PLSDA_plot"))
      ),
      shinydashboard::box(
        width = 5,
        plotly::plotlyOutput(outputId = ns("vipsPlot"))
      )
    ),
    DT::dataTableOutput(outputId = ns("vipsTable"), width = 12)
  )
}

#' PLSDA Server Functions
#'
#' @noRd
mod_PLSDA_server <- function(id, data_NMR_ns, index_metadata, grouping_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    PLSDA_res <- reactive({
      mixOmics::plsda(
        X = data_NMR_ns()[, c(index_metadata() + 1):ncol(data_NMR_ns())],
        Y = data_NMR_ns()[, grouping_var()],
        ncomp = input$PLSy,
        scale = F
      )
    })

    ## get colour palette
    pal <- shiny::reactive({
      colourCount <- length(unique(data_NMR_ns()[, grouping_var()]))
      if (colourCount > 8) {
        getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Set2"))
        pal1 <- getPalette(colourCount)
      } else {
        pal1 <- RColorBrewer::brewer.pal(8, "Dark2")[1:colourCount]
      }
    })
    

    output$PLSDA_plot <- plotly::renderPlotly({
      # NMRMetab_PLS_DA_plot(data_NMR_ns(), groupID = grouping_var(), index_col = index_metadata(), components = c(1,2))
      # comps_scores <- cbind.data.frame("group" = meta_data[, groupID], PLSDA_res()$variates$X[, components], )
      plot1 <- ggplot2::ggplot(as.data.frame(PLSDA_res()$variates$X),
                               ggplot2::aes_string(x = paste0("comp", input$PLSx), 
                                                   y = paste0("comp", input$PLSy))) +
        ggplot2::geom_point(aes(colour = data_NMR_ns()[, grouping_var()])) +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::labs(
          x = paste0("comp", input$PLSx),
          y = paste0("comp", input$PLSy)
        ) +
        ggplot2::guides(colour = guide_legend(title = grouping_var()))+
        ggplot2::scale_color_manual(values = pal())

      if (isTRUE(input$elipses)) {
        plot1 <- plot1 + ggplot2::stat_ellipse(aes(colour = data_NMR_ns()[, grouping_var()]), show.legend = F)
      }
      plot1 <- plotly::ggplotly(plot1, tooltip = "colour")
    })
    
    vips <- reactive({
      vip <- as.data.frame(mixOmics::vip(PLSDA_res()))
    })
    
    output$vipsPlot <- plotly::renderPlotly({
      vipP <- vips() %>% 
        tibble::rowid_to_column("metabolite")
        
      
      p <- ggplot2::ggplot(vipP, ggplot2::aes_string(y = "metabolite",
                                                     x = paste0("comp", input$PLSy)))+
        ggplot2::geom_point(colour = "darkred")+
        ggplot2::theme_bw(base_size = 10)+
        ggplot2::labs(title = "VIP",
                      x = "variable importance in projection")
      p <- plotly::ggplotly(p, tooltip = "y")
      p
        
      })
    
    output$vipsTable <- DT::renderDataTable({
      vips()
    })
    
  })
}

## To be copied in the UI
# mod_PLSDA_ui("PLSDA_ui_1")

## To be copied in the server
# mod_PLSDA_server("PLSDA_ui_1")
