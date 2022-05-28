#' PLSDA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_PLSDA_ui <- function(id){
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
        shiny::plotOutput(outputId = ns("PLSDA_plot"))
      ),
      shinydashboard::box(
        width = 5,
        shiny::plotOutput(outputId = ns("PCA_loading_plot"))
      )
    ),
    DT::dataTableOutput(outputId = ns("table_PC"), width = 12)
  )
}
    
#' PLSDA Server Functions
#'
#' @noRd 
mod_PLSDA_server <- function(id, data_NMR_ns, index_metadata, grouping_var){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    PLSDA_res <- reactive({
      mixOmics::plsda(X = data_NMR_ns()[,c(index_metadata()+1):ncol(data_NMR_ns())],
                      Y = data_NMR_ns()[,grouping_var()], 
                      ncomp = input$PLSy,
                      scale = F)
    })
    
    
    output$PLSDA_plot <- renderPlot({
      #NMRMetab_PLS_DA_plot(data_NMR_ns(), groupID = grouping_var(), index_col = index_metadata(), components = c(1,2))
      #comps_scores <- cbind.data.frame("group" = meta_data[, groupID], PLSDA_res()$variates$X[, components], )
      plot1 <- ggplot2::ggplot(as.data.frame(PLSDA_res()$variates$X), ggplot2::aes_string(paste0("comp", input$PLSx), y = paste0("comp", input$PLSy)))+
        ggplot2::geom_point(aes(colour = data_NMR_ns()[, grouping_var()])) +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::labs(
          x = paste0("comp", input$PLSx),
          y = paste0("comp", input$PLSy)
        ) +
        ggplot2::guides(colour = guide_legend(title = grouping_var()))
      
      if (isTRUE(input$elipses)) {
        plot1 <- plot1 + ggplot2::stat_ellipse(aes(colour = data_NMR_ns()[, grouping_var()]), show.legend = F)
      }
      plot1
      
    })
 
  })
}
    
## To be copied in the UI
# mod_PLSDA_ui("PLSDA_ui_1")
    
## To be copied in the server
# mod_PLSDA_server("PLSDA_ui_1")
