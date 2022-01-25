
NMRMetab_plot_binned <- function(binned_data, index_col = 2, group_var = "sampleID") {
  x <- grp <- y <- NULL
  datgrp <- binned_data %>% dplyr::pull(var = group_var)
  datmatrix <- binned_data[, index_col:ncol(binned_data)]
  dataTemp <- data.frame(
    x = rep(1:ncol(datmatrix), nrow(datmatrix)),
    y = as.vector(t(datmatrix)), grp = factor(rep(datgrp,
      each = ncol(datmatrix)
    ))
  )
  sample_sum <- dataTemp %>%
    dplyr::group_by(x, grp) %>%
    dplyr::summarize(
      mean = mean(y),
      sd = sd(y), mean_p2sd = mean + 2 * sd, mean_m2sd = mean -
        2 * sd
    ) %>%
    dplyr::ungroup()
  p <- ggplot2::ggplot(data = sample_sum, aes(
    x = x, y = mean,
    group = grp, col = grp, stat = "identity"
  )) +
    geom_line()
  p <- p + theme_bw() + ggtitle("NMR bins") + xlab("Bin") +
    ylab("Intensity") + theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 90), legend.position = "none"
    )
  plot(p)
  return(p)
}
