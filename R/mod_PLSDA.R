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
      bs4Dash::box(
        width = 12,
        collapsible = F,
        fluidRow(
          column(
            width = 2,
            numericInput(inputId = ns("PLSx"), label = "compx", value = 1, min = 1, max = 15, step = 1, width = "50%")
            ),
          column(
            width = 2,
            numericInput(inputId = ns("PLSy"), label = "compy", value = 2, min = 1, max = 15, step = 1, width = "50%")
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
        width = 6,
        height = "30em",
        plotOutput(outputId = ns("PLSDA_plot"), 
                   height = "28em",
                   brush = brushOpts(
                     id = ns("PLSDA_brush"))
                   )
      ),
      bs4Dash::box(
        height = "30em", 
        width = 6, 
        DT::dataTableOutput(outputId = ns("PLSDA_table"), 
                            height = "28em"
                            )
        )
      ),
    fluidRow(
      bs4Dash::box(
        width = 6,
        height = "30em",
        fluidRow(
          column(
            width = 4,
            shiny::checkboxInput(inputId = ns("order_VIP"), 
                                 label = "order",
                                 value = F, width = "100%")),
          column(
            width = 4,
            conditionalPanel(condition = "input.order_VIP",
                             ns = ns,
                             shiny::uiOutput(outputId = ns("VIPtoshow_UI"))))
          ),###
        plotly::plotlyOutput(outputId = ns("vipsPlot"),
                             height = "28em")
      ), 
      bs4Dash::box(
        height = "30em", 
        width = 6, 
        DT::dataTableOutput(outputId = ns("vipsTable"),
                            height = "28em"
        )
      )
    ),
    fluidRow(
      bs4Dash::box(
        width = 12,
        column(
          width = 6,
          shiny::checkboxInput(inputId = ns("perfModel"), 
                               label = "Perform Validation",
                               value = F, 
                               width = "50%")
          ),
          conditionalPanel(condition = "input.perfModel",
                           ns = ns,
                             shiny::numericInput(ns("compPeffModel"),
                                                 label = "number of components for the model",
                                                 value = 3,
                                                 min = 1,
                                                 max = 10),
                             
                             actionButton(inputId = ns("GO_validation"), label = "GO")
                             
        )
      )
    ),
    fluidRow(
      bs4Dash::box(
        width = 6,
        height = "30em",
        plotOutput(outputId = ns("CV_plot"), 
                   height = "28em")
        ),
      bs4Dash::box(
        width = 6, 
        height = "30em",
        uiOutput(outputId = ns("pred_factor_UI")),
        DT::dataTableOutput(outputId = ns("prediction_table"), 
                            height = "28em")
        )
      )
  )
}

#' PLSDA Server Functions
#'
#' @noRd
mod_PLSDA_server <- function(id, data_NMR_ns, index_metadata, grouping_var) {
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
    
    ######### RUN PLSDA
    
    PLSDA_res <- reactive({
      mixOmics::plsda(
        X = data_NMR_ns()[, c(index_metadata() + 1):ncol(data_NMR_ns())],
        Y = data_NMR_ns()[, grouping_var()],
        ncomp = input$PLSy,
        scale = F
      )
    })
    
    ######### GET PLSDA SCORES
    PLSDA_scores <- reactive({
      group = data_NMR_ns()[, grouping_var()]
      PLSDA_scores <- cbind("group" = group, as.data.frame(PLSDA_res()$variates$X))
      colnames(PLSDA_scores)[1] = grouping_var()
      PLSDA_scores
    })

    ######## TABLE OF PLSDA SCORES
    output$PLSDA_plot <- renderPlot({
      # NMRMetab_PLS_DA_plot(data_NMR_ns(), groupID = grouping_var(), index_col = index_metadata(), components = c(1,2))
      # comps_scores <- cbind.data.frame("group" = meta_data[, groupID], PLSDA_res()$variates$X[, components], )
      plot1 <- ggplot2::ggplot(PLSDA_scores(),
                               ggplot2::aes_string(x = paste0("comp", input$PLSx), 
                                                   y = paste0("comp", input$PLSy),
                                                   colour = grouping_var())) +
        ggplot2::geom_point() +
        ggplot2::theme_bw(base_size = 16) +
        ggplot2::labs(
          x = paste0("comp", input$PLSx),
          y = paste0("comp", input$PLSy)
        ) +
        ggplot2::scale_color_manual(values = pal())

      if (isTRUE(input$elipses)) {
        plot1 <- plot1 + ggplot2::stat_ellipse(show.legend = F)
      }
      plot1
    })
    
    output$PLSDA_table <- DT::renderDataTable({
      if (is.null(input$PLSDA_brush)) {
        PLSDA_scores()
      } else {
        brushedPoints(PLSDA_scores(), input$PLSDA_brush) 
      }
      
    },
    rownames = F,
    fillContainer = T,
    #filter = 'top',
    options = list(
      autoWidth = FALSE, scrollX = TRUE, scrollY = TRUE, searching = TRUE))
    
    ### ui output to select how many to show
    
    output$VIPtoshow_UI <- shiny::renderUI({
      shiny::numericInput(inputId = ns("VIPtoshow"), 
                          label = "vips", 
                          value = 10,#nrow(ceiling(vips()/2)), 
                          min = 1, 
                          max = 50,
                          step = 1,
                          width = "100%")
    })
    
    vips <- reactive({
      vip <- as.data.frame(mixOmics::vip(PLSDA_res()))
      vip$metabolite <- rownames(vip)
      vip <- vip[,c(paste0("comp", input$PLSy), "metabolite")]
      colnames(vip) <- c("comp", "metabolite")
      vip <- vip[order(-vip$comp),]
      vip
    })
    
    
    output$vipsPlot <- plotly::renderPlotly({
      vipP <- vips()
      if (isFALSE(input$order_VIP)) {
        
        p <- ggplot2::ggplot(vipP, 
                             ggplot2::aes(x = metabolite,
                                          y = comp))+
          ggplot2::geom_point(colour = "darkred")+
          ggplot2::theme_bw(base_size = 10)+
          ggplot2::labs(title = "VIP",
                        x = "variable importance in projection")
        
        
        p <- plotly::ggplotly(p, tooltip = "x") %>% 
          plotly::layout(hovermode = "x unified")
        
      } else if (isTRUE(input$order_VIP)) {
        vipP <- vipP[1:input$VIPtoshow,]
        p <- ggplot2::ggplot(vipP, ggplot2::aes(x = comp,
                                                y = reorder(metabolite, comp)))+
          ggplot2::geom_point(colour = "darkred")+
          ggplot2::theme_bw(base_size = 10)+
          ggplot2::labs(title = "VIP",
                        x = "variable importance in projection", 
                        y = "")
        
        p <- plotly::ggplotly(p, tooltip = "y") 
        
      }
      
      
      p
      
      })
    ### table of VIPs
    output$vipsTable <- DT::renderDataTable({
      vips()
    },
    rownames = F,
    fillContainer = T,
    #filter = 'top',
    options = list(
      autoWidth = FALSE, scrollX = TRUE, scrollY = TRUE, searching = TRUE))
    
    
    ### Run CVs
    cv_res <- eventReactive(input$GO_validation,{
      plsda_res <- mixOmics::plsda(
        X = data_NMR_ns()[, c(index_metadata() + 1):ncol(data_NMR_ns())],
        Y = data_NMR_ns()[, grouping_var()],
        ncomp = input$compPeffModel,
        scale = F
      )
      
      cv_df = mixOmics::perf(object = plsda_res,
                             validation = "loo",
                             #folds = 2, 
                             #nrepeats = CV-repeats, 
                             progressBar = F)
      cv_df = cv_df[['error.rate']] %>%
        lapply(data.frame) %>%
        dplyr::bind_rows(.id = 'type')
      cv_df$comp = c(1:(nrow(cv_df)/2))
      cv_df <- cv_df %>% 
        tidyr::pivot_longer(cols = c(max.dist, centroids.dist, mahalanobis.dist),
                            names_to = "error_type")
      #dplyr::mutate(comp = factor(c(1:(nrow(error.dfs)/2))))
      #cv_df$comp = factor(c(1:(nrow(error.dfs)/2)))
      #error.dfs = reshape2::melt(error.dfs)
      cv_df
    })
    
    output$CV_plot <- renderPlot({
      cv_res() %>% 
        ggplot2::ggplot(ggplot2::aes(x = comp, y = value, colour = error_type, linetype = type))+
        ggplot2::geom_line()+
        labs(x = "Component", y = "Classification error rate")+
        theme_bw()
    })
    
    
    output$pred_factor_UI <- renderUI({
      selectInput(inputId = ns("pred_factor"), 
                  label = "prediction factor",
                  choices = unique(data_NMR_ns()[, grouping_var()]), 
                  multiple = F, 
                  selectize = T)
    })
    
    prediction_table <- eventReactive(input$GO_validation,{
      
      pred_factor <- input$pred_factor
      
      predictions_list <- list()
      pred = list()
      vips = list()
      cvs = list()
      for (i in 1:input$compPeffModel) {
        set.seed(21)
        for (j in 1:5) {
          splits = rsample::initial_split(data = data_NMR_ns(), strata = grouping_var())
          dat_train = rsample::training(splits) %>% 
            NMRMetab_norm_scale(index_col = c(index_metadata() + 1), 
                                scaling = "Pareto")
          
          dat_test = rsample::testing(splits) %>% 
            NMRMetab_norm_scale(index_col = c(index_metadata() + 1), 
                                scaling = "Pareto")
          
          plsda_model <- mixOmics::plsda(X = dat_train[, c(index_metadata() + 1):ncol(dat_train)], 
                                         Y = factor(dat_train[, grouping_var()]), 
                                         ncomp = input$compPeffModel, 
                                         scale = F)
          
          vips[[j]] = mixOmics::vip(plsda_model) %>% 
            as.data.frame() %>% 
            tibble::rownames_to_column("metabolite")
          
          prediction <- tibble::tibble(predictions = factor(stats::predict(plsda_model,
                                                                           dat_test[c(index_metadata() + 1):ncol(dat_test)])$class$mahalanobis.dist[, i]),
                                       true_val = factor(dat_test[, grouping_var()]))
          
          if (all(levels(prediction$true_val) %in% levels(prediction$predictions))) {
            print("all_levels present")
            if (pred_factor != "not_set") {
              prediction <- prediction %>% 
                dplyr::mutate(predictions = relevel(predictions, pred_factor),
                              true_val = relevel(true_val, pred_factor))
            }
            
            pred[[j]] <- prediction %>% yardstick::conf_mat(truth = "true_val", 
                                                            estimate = "predictions") %>% 
              summary() %>%
              dplyr::filter(.metric %in% c("accuracy", "bal_accuracy", 
                                           "precision", "recall", "f_meas")) %>% 
              dplyr::select(-.estimator)
          }
          else {
            print("not all levels present")
            pred[[j]] <- NULL
          }
        }
        predictions_list[[i]] = pred %>% 
          dplyr::bind_rows(.id = "run") %>% 
          dplyr::group_by(.metric) %>% 
          dplyr::summarise(estimate = round(mean(.estimate) * 100, digits = 1),
                           sd = round(sd(.estimate) * 100, digits = 1))
      }
      predictions_list = predictions_list %>% 
        dplyr::bind_rows(.id = "component")
      
      predictions_list
    })
    
    output$prediction_table <- DT::renderDataTable({
      prediction_table()
      
    }, rownames = F,
    fillContainer = T,
    #filter = 'top',
    options = list(
      autoWidth = FALSE, scrollX = TRUE, scrollY = TRUE, searching = TRUE,
      pageLength = 5, 
      lengthMenu = list(c(5, 15, -1), c('5', '10', 'All'))))
  })
}

## To be copied in the UI
# mod_PLSDA_ui("PLSDA_ui_1")

## To be copied in the server
# mod_PLSDA_server("PLSDA_ui_1")

