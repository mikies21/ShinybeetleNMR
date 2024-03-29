

NMRMetab_plot_binned <- function(binned_data, index_col = 2, group_var = "sampleID", title_plot = "NMR bins") {
  
  
  sample_sum <- binned_data %>% 
    dplyr::mutate(dplyr::across(1:c(index_col-1), as.character)) %>% 
    dplyr::group_by(.data[[group_var]]) %>% 
    dplyr::summarise(dplyr::across(where(is.numeric), mean)) 
  
  sample_sum_pivot <- sample_sum %>% 
    tidyr::pivot_longer(cols = !1, names_to = "metabolite", values_to = "mean") 
  
  colnames(sample_sum_pivot)[1] <- "grp"
  metabolites <-unique(sample_sum_pivot$metabolite)
  
  sample_sum_pivot <- split(sample_sum_pivot, f = sample_sum_pivot$grp)
  
  p <- plotly::plot_ly(x = ~metabolites)  %>% 
    plotly::layout(title = title_plot,
                   xaxis = list(title = "Bin", showticklabels = FALSE),
                   yaxis = list(title = "intensity", fixedrange = TRUE),
                   hovermode = "x unified")
  
  for (i in seq_along(sample_sum_pivot)){
    p <- p %>% plotly::add_lines(y = sample_sum_pivot[[i]]$mean, name = sample_sum_pivot[[i]]$grp,
                                 line = list(width = 4))
  }
}

NMRMetab_plot_binned <- function(binned_data, index_col = 2, group_var = "sampleID") {
  sample_sum <- binned_data %>%
    dplyr::mutate(dplyr::across(1:c(index_col - 1), as.character)) %>%
    dplyr::group_by(.data[[group_var]]) %>%
    dplyr::summarise(dplyr::across(where(is.numeric), mean)) %>%
    tidyr::pivot_longer(cols = !1, names_to = "metabolite", values_to = "mean")

  #colnames(sample_sum)[1] <- "grp"
  
  metabolites <- unique(sample_sum$metabolite)
  
  p <- ggplot2::ggplot(sample_sum, 
                       ggplot2::aes_string(x = "metabolite", 
                                           y = "mean",
                                           colour = group_var,
                                           group = group_var))+
    ggplot2::geom_line(stat = "identity")+
    ggplot2::theme_bw(base_size = 10)+
    ggplot2::labs(x = "Bin",
                  y = "intensity",
                  title = "NMR bins")+
    ggplot2::theme(axis.text.x = element_blank())
  
  

  return(list("plot" = p, "sample_sum_df" = sample_sum))
}
