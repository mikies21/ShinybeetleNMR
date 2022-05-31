#' PCA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_PCA_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 1,
        numericInput(inputId = ns("PCx"), label = "PCx", value = 1, min = 0, max = 15, step = 1),
        shinyWidgets::materialSwitch(
          inputId = ns("elipses"),
          label = "add ellipses",
          value = FALSE,
          status = "primary"
        )
      ),
      column(
        width = 1,
        numericInput(inputId = ns("PCy"), label = "PCy", value = 2, min = 1, max = 15, step = 1)
      ),
      shinydashboard::box(
        width = 5,
        plotly::plotlyOutput(outputId = ns("PCA_plot"))
      ),
      shinydashboard::box(
        width = 5,
        plotly::plotlyOutput(outputId = ns("PCA_loading_plot"))
      )
    ),
    DT::dataTableOutput(outputId = ns("table_PC"), width = 12)
  )
}


#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, data_NMR_ns, index_metadata, grouping_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    PCA_res <- reactive({
      PCA <- prcomp(x = data_NMR_ns()[, c(index_metadata() + 1):ncol(data_NMR_ns())], center = F, scale. = F)
    })
    
    output$PCA_plot <- plotly::renderPlotly({
      prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)
      
      if (input$PCx != 0) {
        PCA_scores <- cbind(as.data.frame(PCA_res()$x[,c(input$PCx, input$PCy)]),data_NMR_ns()[, 1:index_metadata()])
        
        ## get colour palette
        colourCount = length(unique(PCA_scores[, grouping_var()]))
        if (colourCount>8) {
          getPalette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))
          pal = getPalette(colourCount)
        } else {
          pal = RColorBrewer::brewer.pal(8, "Dark2")[1:colourCount]
        }
        
        
        
        plot1 <- plotly::plot_ly(colors = pal[1:colourCount]) 
        
        plot1 <- plot1 %>% plotly::add_trace(data = PCA_scores,
                            x = as.formula(paste0('~', "PC", input$PCx)),
                            y = as.formula(paste0('~', "PC", input$PCy)),
                            color = as.formula(paste0('~', grouping_var())),
                            #colors = pal[1:colourCount],
                            type = "scatter",
                            mode = "markers") 
        
        if (isTRUE(input$elipses)) {
          ###calculate elipses
          
          ###### elipses try
          #ellipses_df %>% 
          #  ggplot2::ggplot(ggplot2::aes_string(x = "PC1", y = "PC2", colour = "group"))
          ###########
          
          
          
          gplot <- ggplot2::ggplot(PCA_scores, 
                                   ggplot2::aes_string(x = paste0("PC", input$PCx),
                                                       y = paste0("PC", input$PCy), 
                                                       colour = grouping_var(),
                                                       group = grouping_var()))+
            ggplot2::stat_ellipse() 
          gplot <- ggplot2::ggplot_build(plot = gplot)$data[[1]]
          gplot$group <- as.factor(gplot$group)
    
         
          
          #for (i in seq_along(ellipses_df)){
          #  plot1 <- plot1 %>% 
          #    plotly::add_trace(x= ellipses_df[[i]]$x, y=ellipses_df[[i]]$y,  name =ellipses_df[[i]]$grp,
          #                      type = "scatter", 
          #                      mode = "lines")
          #}
          
          
          plot1 <- plot1 %>% 
            plotly::add_trace(data = gplot, 
                              type = "scatter", 
                              mode = "lines", 
                              x=~x,
                              y=~y,
                              color =~group,
                              #colors = pal[1:colourCount],
                              showlegend = F
                              )
        }
        
        plot1 <- plot1 %>% 
            plotly::layout(title = "PCA scores plot",
                           xaxis = list(title = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = "")),
                           yaxis = list(title = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")))
        
        
        
      } else {
        PCA_scores <- cbind(data_NMR_ns()[, grouping_var()], as.data.frame(PCA_res()$x[, input$PCy]))
        
        colnames(PCA_scores) <- c("grp", "PC")
        
        dens <- with(PCA_scores, tapply(PC,
                                        INDEX = grp,
                                        density))
        df <- data.frame(
          x = unlist(lapply(dens, "[[", "x")),
          y = unlist(lapply(dens, "[[", "y")),
          cut = rep(names(dens), each = length(dens[[1]]$x))
        )
        plot1 <-  plotly::plot_ly(df, x = ~x, y = ~y, color = ~cut) %>%
          plotly::add_lines() %>% 
          plotly::layout(title = "PCA density plot",
                         xaxis = list(title = ""),
                         yaxis = list(title = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = ""), fixedrange = TRUE),
                         hovermode = "x unified")
      }
      #%>% 
      #highlight("plotly_selected", dynamic = TRUE)
      
      #plot1 <- ggplot2::ggplot(as.data.frame(PCA_res()$x), ggplot2::aes_string(x = paste0("PC", input$PCx), y = paste0("PC", input$PCy))) +
      #  ggplot2::geom_point(ggplot2::aes(colour = data_NMR_ns()[, grouping_var()])) +
      #  ggplot2::theme_bw(base_size = 16) +
      #  ggplot2::labs(
      #    x = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = ""),
      #    y = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")
      #  ) +
      #  ggplot2::guides(colour = ggplot2::guide_legend(title = grouping_var()))
      
      plot1
      #plotly::ggplotly(plot1, source = "pointsOfInterest")
    })
    
    
    output$PCA_loading_plot <- plotly::renderPlotly({
      prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)
      
      PCA_loadings <- as.data.frame(PCA_res()$rotation)
      PCA_loadings$metabolites <- rownames(PCA_loadings)
      if (input$PCx != 0) {
        plot2 <- plotly::plot_ly(PCA_loadings,
                               x = as.formula(paste0('~', "PC", input$PCx)),
                               y = as.formula(paste0('~', "PC", input$PCy)),
                               type = "scatter", 
                               mode = "markers") %>% 
        plotly::layout(title = "PCA Loadings plot",
                       xaxis = list(title = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = "")),
                       yaxis = list(title = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")))
      } else {
        plot2 <- plotly::plot_ly(PCA_loadings,
                                 x =~metabolites,
                                 y =as.formula(paste0('~', "PC", input$PCy)),
                                 type = "scatter", 
                                 mode = "markers") %>% 
          plotly::layout(title = "PCA Loadings plot",
                         xaxis = list(title = "bins/metabolites",showticklabels = FALSE),
                         yaxis = list(title = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = ""),fixedrange = TRUE),
                         hovermode = "x unified")
        }
      plot2
    })
    
    
    output$table_PC <- DT::renderDataTable({
      d <- plotly::event_data("plotly_click", source = "pointsOfInterest")
    })
    
  })
}


## To be copied in the UI
# mod_PCA_ui("PCA_ui_1")

## To be copied in the server
# mod_PCA_server("PCA_ui_1")
