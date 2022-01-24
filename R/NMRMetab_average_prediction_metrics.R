
#' @title compute PLS-DA prediction metrics, VIPs
#' @name NMRmetab_average_prediction_metrics
#' @description alot of stuff
#' @param dat a matrix or a data.frame. rows are samples and columns are grouping and metabolites or bins
#' @param groupID a vercors with the column name contining the grouping of the data. This parameter is to be use to mae sure that the 2 new data.frames created have all groups mentioned in the column we identifiy
#' @param index_col name of the column containing the groups
#' @param components_model number of components in the model
#' @param iterations number of iterations. ie number of times you are running the random splits -> plsda -> test prediction
#' @param run_CV boolean. default is false. intensive computer work. performs the CV on each train split for the maximum number of comomponent.
#' @param CV_validation string. either 'loo' default of 'Mfold'
#' @param CV_folds integer. number of folds. only used for Mfold validation
#' @param CV_repeats integer. number of repeats only used for Mfold validation
#' @param pred_factor string. name of the group you want ptredction values. if set to null then only fist factor alphabetically ordered is used.
#' @export


NMRmetab_average_prediction_metrics = function(dat,
                                      groupID = 'disease_status',
                                      pred_factor = NULL,
                                      index_col = 9,
                                      components_model = 5,
                                      iterations = 5,
                                      run_CV = F,
                                      CV_validation = 'loo',
                                      CV_folds = 5,
                                      CV_repeats = 100){
  CV <- repeats <- .metric <- .estimator <- .estimate <- run <- comp <- type <- mahalanobis.dist <- metabolite <- NULL

  if (is.null(pred_factor)) {
    pred_factor <- 'not_set'
  }

  predictions_list <- list()
  pred = list()
  vips = list()
  cvs = list()


  for (i in 1:components_model) {
    set.seed(21)
    for(j in 1:iterations){
      splits = rsample::initial_split(data = dat, strata = groupID)

      dat_train = rsample::training(splits) %>%
        NMRMetab_norm_scale(index_col = index_col, scaling = 'Pareto')

      dat_test = rsample::testing(splits) %>%
        NMRMetab_norm_scale(index_col = index_col, scaling = 'Pareto')



      plsda_model <- mixOmics::plsda(
        X = dat_train[, index_col:ncol(dat_train)],
        Y = factor(dat_train[, groupID]),
        ncomp = components_model, scale = F
      ) ### change number of components appropiately

      if (run_CV & components_model == i) {
        cv_df = mixOmics::perf(object = plsda_model, validation = CV_validation, folds = CV_folds, nrepeats = CV-repeats, progressBar = T)
        cv_df = cv_df[['error.rate']] %>%
          lapply(data.frame) %>%
          dplyr::bind_rows(.id = 'type')
        cv_df$comp = c(1:(nrow(cv_df)/2))
          #dplyr::mutate(comp = factor(c(1:(nrow(error.dfs)/2))))
        #cv_df$comp = factor(c(1:(nrow(error.dfs)/2)))
        #error.dfs = reshape2::melt(error.dfs)
        print(cv_df)
        cvs[[j]] = cv_df
      }

      vips[[j]] = mixOmics::vip(plsda_model) %>%
        as.data.frame() %>%
        tibble::rownames_to_column('metabolite')


      prediction <- tibble::tibble("predictions" = factor(stats::predict(plsda_model,
                                                                        dat_test[index_col:ncol(dat_test)])$class$mahalanobis.dist[, i]),### change number of components appropiately
                                   "true_val" = factor(dat_test[, groupID]))


      #     prediction$prediction = relevel(prediction$prediction, ref = 'Healthy')#
      #     prediction$true_val = relevel(prediction$true_val, ref = 'Healthy')

      ### chack if prediction and thruth have the same number of levels ####

      if (all(levels(prediction$true_val) %in% levels(prediction$predictions))) {
        print('all_levels present')

        if (pred_factor != 'not_set') {
          prediction <- prediction %>% mutate(predictions = relevel(predictions, pred_factor),
                                              true_val = relevel(true_val, pred_factor))
        }

        pred[[j]] <- prediction %>%
          yardstick::conf_mat(truth = "true_val", estimate = "predictions") %>%
          summary() %>%
          dplyr::filter(.metric %in% c("accuracy", "bal_accuracy", "precision", "recall", "f_meas")) %>%
          dplyr::select(-.estimator)

      } else {
        print('not all levels present')
        pred[[j]] <- NULL
      }

    }


    predictions_list[[i]] = pred %>%
      dplyr::bind_rows(.id = "run") %>%
      dplyr::group_by(.metric) %>%
      dplyr::summarise(estimate = round(mean(.estimate) * 100, digits = 1), sd = round(sd(.estimate) * 100, digits = 1))

  }

  predictions_list = predictions_list %>% dplyr::bind_rows(.id ='component')

  names(vips) = c(paste(rep('run',iterations), 1:iterations, sep = ''))
  vips = vips %>%
    dplyr::bind_rows(.id ='run')
  median_vips = vips %>%
    dplyr::select(-run) %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with('comp'),median))
  mean_vips = vips %>%
    dplyr::select(-run) %>%
    dplyr::group_by(metabolite) %>%
    dplyr::summarise(dplyr::across(.cols = dplyr::starts_with('comp'),mean))

  # GET MEAN AND MEDIAN FOR VIPS FOR EACH COMPONENT

  #vips = vips %>%
  #  dplyr::select(run, metabolite, comp2) %>%
  #  tidyr::pivot_wider(names_from = 'metabolite',values_from = 'comp2') %>%
  #  t() %>%
  #  janitor::row_to_names(1) %>%
  #  as.data.frame() %>%
  #  tibble::rownames_to_column('metabolite') %>%
  #  tibble::as_tibble()

#  vips[1:iterations+1] <- sapply(vips[1:iterations+1], as.numeric)
  if (run_CV){
    names(cvs) = c(paste(rep('run',iterations), 1:iterations, sep = ''))
    cvs = dplyr::bind_rows(cvs, .id = 'run')
    p = cvs %>%
      dplyr::select(run, comp, type, mahalanobis.dist) %>%
      dplyr::filter(type == 'BER') %>%
      ggplot2::ggplot(ggplot2::aes(x = comp,
                                   y = mahalanobis.dist,
                                   linetype = type,
                                   colour = run,
      )) +
      ggplot2::geom_line(stat = 'identity')+
      ggplot2::geom_point()+
      ggplot2::theme_bw()

    return(list('predictions' = predictions_list,
                'vips' = vips,
                'CVs' = cvs,
                'cv_plot' = p,
                'median_vips' = median_vips,
                'mean_vips' = mean_vips,
                'Pred_value' = pred_factor))

  }

  return(list('predictions' = predictions_list,
              'vips' = vips,
              'median_vips' = median_vips,
              'mean_vips' = mean_vips,
              'Pred_value' = pred_factor))
}



#' @title compute PLS-DA scores and plot plsda score
#' @name NMRMetab_PLS_DA_plot
#' @description PLSDA score plot
#' @param data a matrix or a data.frame. rows are samples and columns are grouping and metabolites or bins
#' @param groupID a vercors with the column name contining the grouping of the data. This parameter is to be use to mae sure that the 2 new data.frames created have all groups mentioned in the column we identifiy
#' @param index_col name of the column containing the groups
#' @param components a vector with 2 integer values. default set to 1 and 2
#' @param scale does it need scaling
#' @param elipses boolean. add elipsess around the groups? defailt is False
#' @param size_point double. control the size of the plotted points
#' @export


NMRMetab_PLS_DA_plot <- function(data, groupID = 'group',index_col = 3 ,components = c(1,2), scale = F, elipses = F, size_point = 3) {

  compx <- compy <- group <- NULL

  if (scale == T) {
    data = NMRMetab_norm_scale(data = data,index_col = index_col,scaling = 'Pareto')
  }

  meta_data = data[,1:index_col-1]
  matrix = data[,index_col:ncol(data)]
  #scale the data

  model = mixOmics::plsda(X=matrix,
                          Y= meta_data[,groupID],
                          ncomp = max(components),scale = F)

  comps_scores = cbind.data.frame(model$variates$X[,components], 'group' = meta_data[,groupID])

  if (length(components)==3) {
    colnames(comps_scores) <- c('compx', 'compy', 'compz', 'group')

    mycolors <- RColorBrewer::brewer.pal(n = 8,name = 'Dark2')[1:length(unique(comps_scores$group))]
    comps_scores$color <- mycolors[as.numeric(as.factor(comps_scores$group))]

    p <- rgl::plot3d(
      x=comps_scores$compx, y=comps_scores$compy, z=comps_scores$compz,
      col = comps_scores$color,
      type = 's',
      size = size_point,
      radius = size_point,
      xlab= paste('comp', components[1], sep = ''),
      ylab= paste('comp', components[2], sep = ''),
      zlab= paste('comp', components[3], sep = ''),
      box = F)
  } else {
    colnames(comps_scores) <- c('compx', 'compy','group')

    p = ggplot2::ggplot(comps_scores,
                        aes(x= compx,
                            y = compy,
                            colour = group), fill = 'black') +
      ggplot2::geom_point(size = size_point)+
      ggplot2::theme_bw(base_size = 16)+
      ggplot2::scale_color_brewer(palette = 'Dark2')+
      ggplot2::labs(title ='score plot',
                    x = paste('comp', components[1], sep = ''),
                    y = paste('comp', components[2], sep = '')) +
      ggplot2::guides(fill=guide_legend(title=groupID))
    if (elipses == T) {
      p = p + ggplot2::stat_ellipse(aes(x = compx, y = compy, colour = group))
    }
  }
  return(p)
}
