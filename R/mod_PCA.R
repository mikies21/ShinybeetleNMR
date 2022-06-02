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
        width = 2,
        radioButtons(inputId = ns("PCplotType"), label = "plot Type", choices = c("2d","3d", "corrplot"), selected = "2d", inline = T),
        conditionalPanel(
          condition = "input.PCplotType == 'corrplot'",
          ns = ns,
          numericInput(inputId = ns("maxPC"), label = "PC to plot ", value = 3, min = 2, max = 15, step = 1)
        ),
        conditionalPanel(
          condition = "input.PCplotType == '2d'",
          ns = ns,
          fluidRow(
            column(
              width = 6,
              numericInput(inputId = ns("PCx"), label = "PCx", value = 1, min = 0, max = 15, step = 1),
              shinyWidgets::materialSwitch(
                inputId = ns("elipses"),
                label = "add ellipses",
                value = FALSE,
                status = "primary"
              )
            ),
            column(
              width = 6,
              numericInput(inputId = ns("PCy"), label = "PCy", value = 2, min = 1, max = 15, step = 1)
            )
          )
        ),
        conditionalPanel(
          condition = "input.PCplotType == '3d'",
          ns = ns,
          fluidRow(
            column(
              width = 4,
              numericInput(inputId = ns("PCx"), label = "PCx", value = 1, min = 1, max = 15, step = 1)
            ),
            column(
              width = 4,
              numericInput(inputId = ns("PCy"), label = "PCy", value = 2, min = 1, max = 15, step = 1)
            ),
            column(
              width = 4,
              numericInput(inputId = ns("PCz"), label = "PCz", value = 3, min = 1, max = 15, step = 1)
            )
            
          )
          )
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
    fluidRow(
      column(
        width = 12,
        plotly::plotlyOutput(outputId = ns("explained_var_plot")))
    )
    #DT::dataTableOutput(outputId = ns("table_PC"), width = 12)
  )
}


#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, data_NMR_ns, index_metadata, grouping_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    PCA_res <- shiny::reactive({
      PCA <- prcomp(x = data_NMR_ns()[, c(index_metadata() + 1):ncol(data_NMR_ns())], center = F, scale. = F)
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
    
    ### get explained variances
    prop_var <- reactive({
      round(summary(PCA_res())$importance[2, ] * 100, digits = 3)
      })

    output$PCA_plot <- plotly::renderPlotly({
      
        prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)
        
        if (input$PCplotType != "3d") {

        if (input$PCplotType == "2d") {
          # PCA as points
          if (input$PCx != 0) {
            PCA_scores <- cbind(as.data.frame(PCA_res()$x[, c(input$PCx, input$PCy)]), data_NMR_ns()[, 1:index_metadata()])
            
            plot1 <- ggplot2::ggplot(
              PCA_scores,
              ggplot2::aes_string(
                x = paste0("PC", input$PCx),
                y = paste0("PC", input$PCy),
                colour = grouping_var()
                )
              ) +
              ggplot2::geom_point()
            if (isTRUE(input$elipses)) {
            ### calculate elipses

            ###### elipses try
            # ellipses_df %>%
            #  ggplot2::ggplot(ggplot2::aes_string(x = "PC1", y = "PC2", colour = "group"))
            ###########
              plot1 <- plot1 + ggplot2::stat_ellipse(
                ggplot2::aes_string(
                  x = paste0("PC", input$PCx),
                  y = paste0("PC", input$PCy),
                  colour = grouping_var()
                ),
                show.legend = F
              )
            }

            plot1 <- plot1 +
              ggplot2::theme_bw(base_size = 10) +
              ggplot2::labs(
                title = "PCA scores plot",
                x = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = ""),
                y = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")
              ) +
              ggplot2::theme(legend.title = ggplot2::element_blank())
          } else {
            PCA_scores <- cbind(data_NMR_ns()[, grouping_var()], as.data.frame(PCA_res()$x[, input$PCy]))

            colnames(PCA_scores) <- c("grp", "PC")

            plot1 <- ggplot2::ggplot(
              PCA_scores,
              ggplot2::aes(x = PC, colour = grp)
            ) +
              ggplot2::geom_density() +
              ggplot2::theme_bw(base_size = 10) +
              ggplot2::labs(
                title = "PCA density plot",
                x = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = ""),
                y = "density"
              ) +
              ggplot2::theme(legend.title = ggplot2::element_blank())
          }
        } else if (input$PCplotType == "corrplor") {
          PCA_scores <- cbind(as.data.frame(PCA_res()$x[, 1:input$maxPC]), data_NMR_ns()[, 1:index_metadata()])
            plot1 <- GGally::ggpairs(PCA_scores, 
                            mapping = ggplot2::aes_string(colour = grouping_var()),
                            columns = 1:input$maxPC
                            )+
              ggplot2::theme_bw(base_size = 10)+
              ggplot2::labs(title = "PCA score correlogram")+
              ggplot2::scale_fill_manual(values = pal())
          }
        plot1 <- plot1 +
          ggplot2::scale_color_manual(values = pal())

        plot1 <- plotly::ggplotly(plot1)
        plot1
        } else {
        
        PCA_scores <- cbind(as.data.frame(PCA_res()$x[, c(input$PCx, input$PCy, input$PCz)]), data_NMR_ns()[, 1:index_metadata()])
        
        plot1 <- plotly::plot_ly(data = PCA_scores,
                                 x = as.formula(paste0("~PC", input$PCx)),
                                 y = as.formula(paste0("~PC", input$PCy)),
                                 z = as.formula(paste0("~PC", input$PCz)),
                                 color = as.formula(paste0("~", grouping_var())),
                                 colors = pal()) %>%
          plotly::add_markers()
        plot1
        }
        # plotly::ggplotly(plot1, source = "pointsOfInterest")
      })


      output$PCA_loading_plot <- plotly::renderPlotly({
        
        
          
        if (input$PCplotType == "2d") {
          
        prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)

        PCA_loadings <- as.data.frame(PCA_res()$rotation)
        PCA_loadings$metabolites <- rownames(PCA_loadings)
        if (input$PCx != 0) {
          plot2 <- ggplot2::ggplot(
            PCA_loadings,
            ggplot2::aes_string(
              x = paste0("PC", input$PCx),
              y = paste0("PC", input$PCy),
              text = "metabolites"
            )
          ) +
            ggplot2::geom_point() +
            ggplot2::theme_bw(base_size = 10) +
            ggplot2::labs(
              title = "PCA loadings plot",
              x = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = ""),
              y = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")
            )
        } else {
          plot2 <- ggplot2::ggplot(
            PCA_loadings,
            ggplot2::aes_string(
              x = "metabolites",
              y = paste0("PC", input$PCy),
              text = "metabolites"
            )
          ) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::labs(
              title = "PCA Loadings plot",
              x = "bins/metabolites",
              y = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)")
            ) +
            theme_bw(base_size = 10) +
            ggplot2::theme(axis.text.x = ggplot2::element_blank())
        }
        plot2 <- plotly::ggplotly(plot2)
        
      } else if (input$PCplotType == "corrplot") {
        
        PCA_loadings <- as.data.frame(PCA_res()$rotation)
        PCA_loadings$metabolites <- rownames(PCA_loadings)
        plot2 <- GGally::ggpairs(PCA_loadings,
                                 columns = 1:input$maxPC,
                                 mapping = ggplot2::aes_string(text = "metabolites")
        ) +
          ggplot2::theme_bw(base_size = 10) +
          ggplot2::labs(title = "PCA loadings correlogram")
        
        plot2 <- plotly::ggplotly(plot2) 
        plot2
        } else if (input$PCplotType == "3d") {
          PCA_loadings <- as.data.frame(PCA_res()$rotation)
          PCA_loadings$metabolites <- rownames(PCA_loadings)
          
          plot2 <- plotly::plot_ly(data = PCA_loadings,
                                   x = as.formula(paste0("~PC", input$PCx)),
                                   y = as.formula(paste0("~PC", input$PCy)),
                                   z = as.formula(paste0("~PC", input$PCz)),
                                   text =~metabolites,
                                   color = I("black")) %>%
            plotly::add_markers()
          #%>% 
           # plotly::layout(hoverinfo = "text")
          plot2
        }
        
      })
        
      output$explained_var_plot <- plotly::renderPlotly({
        prop_var_df <- data.frame(exp_var = prop_var(),
                                  dimensions = 1:length(prop_var()))
        
        p <- ggplot2::ggplot(prop_var_df, ggplot2::aes(x = dimensions,
                                                       y = exp_var,
                                                       group = 1))+
          ggplot2::geom_point(size = 2)+
          ggplot2::geom_line(show.legend = F)+
          ggplot2::geom_col(show.legend = F)+
          ggplot2::labs(title = "Scree plot",
                        x = "Dimensions",
                        y = "% of explained variance")+
          ggplot2::theme_bw(base_size = 10)
        
        p <- plotly::ggplotly(p, tooltip = c("y", "x")) %>% 
        plotly::layout(hovermode = "x unified")
        
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
