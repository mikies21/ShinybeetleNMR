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
      bs4Dash::box(
        width = 12,
        fluidRow(
          column(
            width = 2,
            numericInput(inputId = ns("PCx"), label = "PCx", value = 1, min = 0, max = 15, step = 1)
            ),
          column(
            width = 2,
            numericInput(inputId = ns("PCy"), label = "PCy", value = 2, min = 1, max = 15, step = 1)
          ),
          column(
            width = 2,
            shinyWidgets::materialSwitch(
              inputId = ns("elipses"),
              label = "add ellipses",
              value = FALSE,
              status = "primary"
              )
            )
          )
        )
      ),
    fluidRow(
      bs4Dash::box(
        height = "30em",
        width = 6, 
        plotOutput(outputId = ns("PCA_plot"), 
                   height = "28em",
                   #click = ns("PCA_click"),
                   brush = brushOpts(
                     id = ns("PCA_brush"))
                   )
        ),
        bs4Dash::box(
          height = "30em",
          width = 6,
          DT::dataTableOutput(outputId = ns("table_PC"),
                              height = "28em")
        )
      ),
    fluidRow(
      bs4Dash::box(
        maximizable = T,
        height = "30em",
        width = 6,
        plotOutput(outputId = ns("PCA_loading_plot"),
                   height = "28em",
                   brush = brushOpts(
                     id = ns("loadings_brush"))
                   )
      ),
      bs4Dash::box(
        height = "30em",
        width = 6,
        DT::dataTableOutput(outputId = ns("table_loadings"),
                            height = "28em")
      )
    ),
    fluidRow(
      column(
        width = 12,
        sliderInput(inputId = ns("sliderPC"),
                    label = "number of Principal Components", 
                    min = 1, 
                    max = 30, 
                    value = 10,
                    step = 1),
        plotOutput(outputId = ns("explained_var_plot"))
      )
    )
  )
}


#' PCA Server Functions
#'
#' @noRd
mod_PCA_server <- function(id, data_NMR_ns, index_metadata, grouping_var) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
    
    #### PERFORM PCA
    PCA_res <- shiny::reactive({
      PCA <- prcomp(x = data_NMR_ns()[, c(index_metadata() + 1):ncol(data_NMR_ns())], center = F, scale. = F)
    })

    ### get explained variances
    prop_var <- reactive({
      round(summary(PCA_res())$importance[2, ] * 100, digits = 3)
    })
    
    ### GET PCA SCORES FOR PLOTTING
    PCA_scores <- reactive({
      cbind(as.data.frame(PCA_res()$x[, c(input$PCx, input$PCy)]), data_NMR_ns()[, 1:index_metadata()])
    })

    ### PLOT PCA SCORES
    
    output$PCA_plot <- renderPlot({
      prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)
      # PCA as points
      if (input$PCx != 0) {
        PCA_scores <- PCA_scores()

        plot1 <- ggplot2::ggplot(
          PCA_scores,
          ggplot2::aes_string(
            x = paste0("PC", input$PCx),
            y = paste0("PC", input$PCy),
            text = grouping_var(),
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
              text = grouping_var(),
              colour = grouping_var()
            ),
            show.legend = F
          )
        }

        plot1 <- plot1 +
          ggplot2::labs(
            title = "PCA scores plot",
            x = paste0("PC", input$PCx, " (", prop_var[input$PCx], "%)", sep = ""),
            y = paste0("PC", input$PCy, " (", prop_var[input$PCy], "%)", sep = "")
          ) 
      } else {
        PCA_scores <- cbind(data_NMR_ns()[, grouping_var()], as.data.frame(PCA_res()$x[, input$PCy]))

        colnames(PCA_scores) <- c("grp", "PC")

        plot1 <- ggplot2::ggplot(
          PCA_scores,
          ggplot2::aes(x = PC, colour = grp)
        ) +
          ggplot2::geom_density() +
          ggplot2::labs(
            title = "PCA density plot",
            x = paste0("PC", input$PCx, " (", prop_var[input$PCy], "%)", sep = ""),
            y = "density"
          ) 
      }
      plot1 +
        ggplot2::theme_bw(base_size = 10) +
        ggplot2::scale_color_manual(values = pal())
      # plotly::ggplotly(plot1, source = "pointsOfInterest")
    })


    PCA_loadings <- reactive({
      PCA_loadings <- as.data.frame(PCA_res()$rotation)
      PCA_loadings$metabolites <- rownames(PCA_loadings)
      PCA_loadings
      })
    
    output$PCA_loading_plot <- renderPlot({
      
      prop_var <- round(summary(PCA_res())$importance[2, ] * 100, digits = 2)
      
      PCA_loadings <- PCA_loadings()
      
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
      plot2
    })

    output$explained_var_plot <- renderPlot({
      prop_var_df <- data.frame(
        exp_var = prop_var(),
        dimensions = 1:length(prop_var())
      ) %>% 
        dplyr::filter(dimensions <= input$sliderPC)

      p <- ggplot2::ggplot(prop_var_df, ggplot2::aes(
        x = dimensions,
        y = exp_var,
        group = 1,
        label = exp_var,
        text = exp_var
      )) +
        ggplot2::geom_point(size = 2) +
        ggplot2::geom_line(show.legend = F) +
        ggplot2::geom_col(show.legend = F) +
        ggplot2::labs(
          title = "Scree plot",
          x = "Dimensions",
          y = "% of explained variance"
        ) +
        ggplot2::theme_bw(base_size = 10) +
        ggrepel::geom_label_repel(show.legend = F)

      p 
    })

    output$table_PC <- DT::renderDataTable({
      if (is.null(input$PCA_brush)) {
        PCA_scores()
      } else {
        brushedPoints(PCA_scores(), input$PCA_brush)
      }
    },
    rownames = F,
    fillContainer = T,
    #filter = 'top',
    options = list(
      autoWidth = FALSE, scrollX = TRUE, scrollY = TRUE, searching = TRUE)
    )
    
    output$table_loadings <- DT::renderDataTable({
      
      
      if (is.null(input$loadings_brush)) {
        if (input$PCx == 0) {
          PCA_loadings()[, c("metabolites", paste0("PC", input$PCy))]
        } else {
          PCA_loadings()[, c("metabolites",paste0("PC", input$PCx), paste0("PC", input$PCy)) ]
        }
      } else {
        loadings <- brushedPoints(PCA_loadings(), input$loadings_brush)
        loadings[, c("metabolites",paste0("PC", input$PCx), paste0("PC", input$PCy))]
      }
    },
    rownames = F,
    fillContainer = T,
    #filter = 'top',
    extensions = 'FixedColumns',
    options = list(
      autoWidth = FALSE, scrollX = TRUE, scrollY = TRUE, searching = TRUE,
      fixedColumns = list(leftColumns = 1)
      )
    )
    
  })
}


## To be copied in the UI
# mod_PCA_ui("PCA_ui_1")

## To be copied in the server
# mod_PCA_server("PCA_ui_1")
