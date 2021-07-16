#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

options(shiny.maxRequestSize = 300 * 1024^2, repos = BiocManager::repositories(version = 3.13))


shinyServer(function(input, output) {
  

  # upload NMR data ---------------------------------------------------------

  dataUpload <- reactive({
    file <- input$fileUpload
    if (is.null(file))
      return(read_csv('example_data/DATA_.csv' ))
    
    read_csv(file$datapath)
    
  })
  
  metadata <- reactive({
    names_to_filter <- colnames(dataUpload())[1:input$metadata_index]
    return(names_to_filter)
  })
  
  metadata1 <- reactive({
    names_to_filter <- colnames(dataUpload())[1:input$metadata_index]
    x <- as.list(names_to_filter)
    #names(x) <- names_to_filter
    return(x)
  })
 
  
  output$textUpload <- renderInfoBox({
    if (is.null(dataUpload())) {
      text_infodata <- paste0(' upload NMR data')
    }
    else {
      text_infodata <- h5(HTML(paste0('data uploaded has ', nrow(dataUpload()), ' samplse and ', ncol(dataUpload()), ' bins', br(),
                                 'The first ', input$metadata_index, ' contain the metadata group information')))
    }
    infoBox(title = 'data', value = text_infodata, color = 'red', icon = icon('list'), fill = T, width = 5)
  })
  
  output$ns_info <- renderInfoBox({
    ns_info <- h5(HTML(paste0('data has been nomalised by ', input$normalisation, ' and then scaled with ', input$scaling)))
    infoBox(title = 'NS', value = ns_info, icon = icon('cubes'), color = 'blue', fill = T)
  })
    #import_file_server(id = "new_upload", btn_show_data = F, trigger_return = "change", return_class = "data.frame")
  #varUpload <- update_variables_server("var_update", data = dataUpload)

# normalisation ---------------------------------------------------------------------------------------------------

  
  dataUpload1 <- reactive({
    if (input$norm_before) {
      df <- NMRMetab_norm_scale(data = dataUpload(), index_col = input$metadata_index + 1, normalisation = input$normalisation)
    }
    else {
      df <- dataUpload()
    }
    
    #col_names <- colnames(df)[1:input$metadata_index]
    #df[,col_names] <- data.frame(apply(df[col_names], 2, as.factor))
    return(df)
  })

# filter dataframe -------------------------------------------------------------------------------------------------


  filtered_data <- filter_data_server(id = "filtering_data", data = dataUpload1, vars = metadata1)

  
  
  data_normalised <- reactive({
    if (input$norm_before) {
      df_norm <- filtered_data$filtered()
    }
    else {
      df_norm <- NMRMetab_norm_scale(data = filtered_data$filtered(), index_col = input$metadata_index + 1, normalisation = input$normalisation)
    }
  })

  data_ns <- reactive({
    df_ns <- NMRMetab_norm_scale(data = data_normalised(), index_col = input$metadata_index + 1, scaling = input$scaling)
  })

  output$df1 <- DT::renderDataTable({
    data_ns()
    #filtered_data$filtered()
  }, options = list(pageLength = 5, info = FALSE))




# download data ------------------------------------------------------------



  output$download_data_ns <- downloadHandler(
    filename = function() {
      paste("data_", input$normalisation, input$scaling, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_ns(), file)
    }
  )


  # plot spectra ------------------------------------------------------------

  output$spectra_groupUI <- renderUI({
    pickerInput("grouping_spectra", label = "grouping variable", choices = metadata(), multiple = F)
  })

  #output$plot_none <- renderPlot({
    #NMRMetab_plot_binned(binned_data = filtered_data$filtered(), index_col = input$metadata_index[2] + 1, group_var = input$grouping_spectra1) +
   #   geom_line(linetype = input$linetype_spectra, size = input$linesize_spectra)
  #})
  #output$plot_norm <- renderPlot({
   # NMRMetab_plot_binned(binned_data = data_normalised(), index_col = input$metadata_index[2] + 1, group_var = input$grouping_spectra1) +
    #  geom_line(linetype = input$linetype_spectra, size = input$linesize_spectra)
  #})
  output$spectra_plot_scaled <- renderPlot({
    NMRMetab_plot_binned(binned_data = data_ns(), index_col = input$metadata_index + 1, group_var = input$grouping_spectra) +
      geom_line()
      #geom_line(linetype = input$linetype_spectra, size = input$linesize_spectra)
  })

  # univariate analysis -----------------------------------------------------


  # univariate <- mod_univ_analysis_server('univ_analysis', NMRdata = data_ns[,input$metadata_index:ncol(data_ns)], metadata = metadata)

  output$univ_groupUI <- renderUI({
    pickerInput("univ_group", label = "grouping variable", choices = metadata(), multiple = F)
  })
  
  grouping_factor <- reactive({
    nlevs <- nlevels(as.factor(data_ns()[,input$univ_group]))
    levs <- levels(as.factor(data_ns()[,input$univ_group]))
    return(list('nlevs' = nlevs, 'levs' = levs))
  })
  
  pair <- reactive({
    if('paired' %in% input$univ_test) { 
      pair = T
      }  else {
        pair = F
      }
    
    pair
    })
  normal <- reactive({
    if('normal' %in% input$univ_test) { 
      normal = T
    }
    else {
      normal = F
    }
    normal
  })
  
  variance <- reactive({
    if('variance' %in% input$univ_test) { 
      variance = T
    }
    else {
      variance = F
    }
    variance
  })
  
  
  
  output$textUniv <- renderInfoBox({
    #nlevs <- nlevels(as.factor(data_ns()[,input$univ_group]))
    #levs <- levels(as.factor(data_ns()[,input$univ_group]))
    univ_info <- h5(HTML(paste0('grouping selected has ', grouping_factor()$nlevs , ' factors: ', paste(grouping_factor()$levs, collapse = ', '))))
    infoBox(title = 'factors', value = univ_info, icon = icon('cubes'), color = 'red', fill = T)
  })
  
  
  output$univariateUI <- renderUI({
    
    if (grouping_factor()$nlevs < 3) {
      checkboxGroupButtons(
        inputId = "univ_test",
        label = "univariate analysis options",
        choices = list("paired groups" = 'paired', 
                       "normal" = 'normal',
                       "equal variance" = 'variance'),
        status = 'danger',
        direction = 'vertical'
      )
    }
    else {
      pickerInput(inputId = "univ_test", 
                  label = "univariate analysis options",
                  choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                  selected = c('BH'))
    }
  })
  
  univariate_test <- reactive({
    ngroup <- match(input$univ_group, colnames(data_ns()))
    df <- as.data.frame(data_normalised())
    df <- df[,c(ngroup, c(input$metadata_index+1):ncol(data_ns()))] %>% rownames_to_column('ID')
    if (grouping_factor()$nlevs < 3) {
      table1 <- NMRMetab_UnivarTest(data = as.data.frame(df), paired = pair(), normality = normal(), equal.variance = variance())
      out <- tibble(table1$out)
      test <- table1$test
      return(list('test' = test, 'out' = out))
    }
    else {
      table1 <- NMRMetab_anova(data = as.data.frame(df), adjMethod = input$univ_test)$anova_pvals %>% rownames_to_column('metabolite')
      out <- table1
      test <- 'ANOVA TEST PERFOMED'
      return(list('test' = test, 'out' = out))
      return(table1)
    }
  })
  
  output$univ_type <- renderInfoBox({
    infoBox(title = 'test', value = univariate_test()$test,icon = icon('cubes'), fill = T, color = 'blue')
  })

  output$univ_table <- DT::renderDataTable({
    univariate_test()$out
  })
  
  

  # PCA ---------------------------------------------


  output$PCA_groupUI <- renderUI({
    pickerInput(inputId = "PCA_group", label = "grouping factor", choices = metadata(), multiple = F, selected = metadata()[1])
  })
  output$PCA_scores_labelsUI <- renderUI({
    pickerInput(inputId = "PCA_scores_labels", label = "label factor", choices = c("none", metadata()), multiple = F)
  })

  PCA <- reactive({
    PCA <- prcomp(as.data.frame(data_ns())[c(input$metadata_index+1):ncol(data_ns())], center = F, scale. = F)
   
  })
  
  output$PCA_ellipsesUI <- renderUI({
    if (input$PCAcorr == 'corrplot') {
      return(NULL)
    }
    else {
      checkboxInput('PCA_ellipses', label = 'add ellipses',value = F)
    }
  })

  PCA_scores = reactive({
    pcs <- c(input$pcx, input$pcy)
    scores <- as.data.frame(PCA()$x) ## just taking the first 20 comp. no need to go higher
    prop_var <- round(summary(PCA())$importance[2, ] * 100, digits = 2)
    prop_var <- prop_var[pcs]
    col_group <- as.data.frame(data_ns())[1:input$metadata_index]
    scores <- cbind.data.frame(scores, col_group)
    if (input$PCAcorr == 'simple') {
      if (input$pcx != input$pcy) {
        plot1 <- ggplot2::ggplot(scores, aes_string(x = paste0('PC', pcs[1]), y = paste0('PC', pcs[2]), colour = input$PCA_group)) +
          ggplot2::geom_point(size = 3)+
          ggplot2::theme_bw(base_size = 16) +
          ggplot2::labs(x = paste0('PC', pcs[1], ' (', prop_var[1], '%)'), y = paste0('PC', pcs[2], ' (', prop_var[2], '%)'))
        if (input$PCA_ellipses) {
          plot1 <- plot1 +
            ggplot2::stat_ellipse(aes_string(colour = input$PCA_group, fill = input$PCA_group),geom = "polygon", alpha = 0.1)
        }
      }
      else {
        plot1 <- ggplot2::ggplot(scores, aes_string(x = paste0('PC', pcs[1]), colour = input$PCA_group)) +
          ggplot2::geom_density(alpha = 0.2) +
          ggplot2::theme_bw(base_size = 16) +
          ggplot2::labs(x = paste0('PC', pcs[1], ' (', prop_var[1], '%)'))
      }
    }
    else if (input$PCAcorr == 'corrplot') {
      plot1 <- GGally::ggpairs(scores, columns = 1:5, ggplot2::aes_string(colour = input$PCA_group))+
        theme_bw(base_size = 7)
    }
    
    plot1
  })

  
  output$PCA_scores <- renderPlot({
    PCA_scores()
  })
  
  PCA_loading <- reactive({
    pcs <- c(input$pcx, input$pcy)
    prop_var <- round(summary(PCA())$importance[2, ] * 100, digits = 2)
    prop_var <- prop_var[pcs]
    loadings <- as.data.frame(PCA()$rotation) %>% rownames_to_column('bin')
    if (input$PCAcorr == 'simple') {
      if (input$pcx != input$pcy) {
        plot2 <- ggplot2::ggplot(loadings, aes_string(x = paste0('PC', pcs[1]), y = paste0('PC', pcs[2]), label = 'bin')) +
        ggplot2::geom_point() +
        ggplot2::theme_bw(base_size = 16) #+
      #ggplot2::labs(title = "PCA loadings", x = paste0('PC', pcs[1], ' (', prop_var[1], '%)'), y = paste0('PC', pcs[2], ' (', prop_var[2], '%)'))
      }
      else {
        plot2 <- ggplot2::ggplot(loadings, aes_string(x = paste0('PC', pcs[1]))) +
          ggplot2::geom_density() +
          ggplot2::theme_bw(base_size = 16) +
          ggplot2::labs(x = paste0('PC', pcs[1], ' (', prop_var[1], '%)'))
      }
    }
    else if (input$PCAcorr == 'corrplot') {
      plot2 <- GGally::ggpairs(loadings, columns = 2:6)+
        theme_bw(base_size = 7)
    }
    
    plot2
    
  })

  output$PCA_loading <- renderPlot({
    PCA_loading()
  })

  # selected_brush <- reactive({
  # dat <- data_normalised() %>% pivot_longer(cols = input$metadata_index[2]+1:ncol(data_normalised()),values_to = 'metabolite')
  #  loading_points <- brushedPoints(df = dat, input$plot_brush)
  #  loading_points
  # })

  brushed_PCA <- reactive({
    brushedPoints(df = PCA_scores()$data, brush = input$PCA_scores_brush)
  })

  output$PCA_info <- DT::renderDataTable({
    if(nrow(brushed_PCA()) == 0 || input$PCAcorr == 'corrplot'){
      return(NULL)
    }
    brushed_PCA()[,c(ncol(brushed_PCA())-input$metadata_index+1):ncol(brushed_PCA())]
    # selected_rows
  })

  brushed_loadings <- reactive({
    brushedPoints(df = PCA_loading()$data, brush = input$PCA_loading_brush)
  })

  output$info <- DT::renderDataTable({
    
    brushed_loadings()
    # selected_rows
  })


  output$boxplots_loadings <- renderPlot({
    if(nrow(brushed_loadings()) == 0 || input$PCAcorr == 'corrplot'){
      return(NULL)
    }
    selected_rows <- c(metadata(), brushed_loadings()$bin)
    new_df <- data_normalised()
    new_df %>%
      dplyr::select(all_of(selected_rows)) %>% 
      pivot_longer(cols = brushed_loadings()$bin, names_to = "Metabolite", values_to = "value") %>%
      ggplot(aes(x = .data[[input$PCA_group]], y = value, fill = .data[[input$PCA_group]])) +
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(width = 0.1) +
      facet_wrap(~Metabolite, scales = "free_y") +
      theme_bw(base_size = 13)
  })


  # PLS-DA ------------------------------------------
  output$PLS_groupUI <- renderUI({
    pickerInput(inputId = "PLS_group", label = "grouping factor", choices = metadata(), multiple = F, selected = metadata()[1])
  })
  
  PLSDA <- reactive({
    df <- as.data.frame(data_ns())
    matrix <- df[,c(input$metadata_index+1):ncol(df)]
    factor_data <- df %>% pull(input$PLS_group)
    
    
    model = mixOmics::plsda(X= matrix,
                            Y= factor_data,
                            ncomp = 10,
                            scale = F)
    return(model)
  })
  output$PLS_scores <- renderPlot({
    comps_scores = tibble(as.data.frame(PLSDA()$variates$X[,c(input$compx, input$compy)]), 'group' = PLSDA()$Y)
    p = ggplot2::ggplot(comps_scores,
                        aes_string(x = paste0('comp', input$compx),
                                   y = paste0('comp', input$compy),
                                   colour = 'group')) +
      ggplot2::geom_point(size = 3)+
      ggplot2::theme_bw(base_size = 16)+
      ggplot2::scale_color_brewer(palette = 'Dark2')+
      ggplot2::labs(title ='PLS-DA score plot') +
      ggplot2::guides(colour=guide_legend(title=input$PLS_group))
    if (input$PLS_ellipse == T) {
      p = p + ggplot2::stat_ellipse(aes_string(x = paste0('comp', input$compx),
                                               y = paste0('comp', input$compy),
                                               colour = 'group'))
    }
    p
    })
  
  output$PLS_plotvar <- renderPlot({
    plotVar(PLSDA(),var.names = T, style = 'ggplot2')
  })

  output$PLS_group_trainUI <- renderUI({
    pickerInput(inputId = "PLS_group_train", label = "grouping factor", choices = metadata(), multiple = F, selected = metadata()[1])
  })
  

# PLS-DA test train -------------------------------------------------------

  
  testtrain <- reactive({
    splits <- rsample::initial_split(data = data_normalised(), strata = input$PLS_group_train, prop = input$testtrainsplit/100)
    train <- rsample::training(splits) %>%
      NMRMetab_norm_scale(index_col = input$metadata_index + 1, normalisation = input$normalisation)
    test <- rsample::testing(splits) %>%
      NMRMetab_norm_scale(index_col = input$metadata_index + 1, normalisation = input$normalisation)
    return(list('train' = train, 'test' = test))
    })
  
  PLS_train <- reactive({
    plsda_model <- mixOmics::plsda(
      X = testtrain()$train[, c(input$metadata_index+1):ncol(testtrain()$train)],
      Y = factor(testtrain()$train[, input$PLS_group_train]),
      ncomp = input$ncomp, scale = F
      )
    plsda_model
  })
  
  CV <- reactive({
    cv_df <-  mixOmics::perf(object = PLS_train(), validation = input$CV_validation)
    cv_df
  })
  
  output$CV_plot <- renderPlot({
    plot(CV())
  })
  
  PLS_results <- eventReactive(input$do_PLS, {
    NMRmetab_average_prediction_metrics(dat = data_ns(), groupID = input$PLS_grouping1, index_col = input$metadata_index[2] + 1, components_model = 3, iterations = 1, run_CV = F)
  })

  output$metrics_table <- DT::renderDataTable({
    PLS_results()$predictions
  })
  
  #addd new things here
})
