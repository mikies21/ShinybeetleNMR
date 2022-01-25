


NMRMetab_PCA_plot <- function(data, groupID, index_col = 2, elipses = F, pcs = c(1, 2), size_point = 3) {
  PCA <- prcomp(data[index_col:ncol(data)], center = F, scale. = F)

  drugs_scores <- as.data.frame(PCA$x[, pcs]) ## just taking the first 20 comp. no need to go higher
  prop_var <- round(summary(PCA)$importance[2, ] * 100, digits = 2)
  prop_var <- prop_var[pcs]
  PCnames <- colnames(drugs_scores)[pcs]
  PCnamex <- paste(PCnames[1], " (", prop_var[1], "%)", sep = "")
  PCnamey <- paste(PCnames[2], " (", prop_var[2], "%)", sep = "")
  col_group <- data[1:index_col - 1]



  drugs_scores <- cbind.data.frame(col_group, drugs_scores)
  # colnames(drugs_scores) <- c('PCx','PCy')
  # drugs_scores = cbind.data.frame(group = )
  plot1 <- ggplot2::ggplot(drugs_scores, aes_string(x = PCnames[1], y = PCnames[2], colour = groupID)) +
    ggplot2::geom_point(size = size_point) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::labs(x = PCnamex, y = PCnamey)

  if (elipses == T) {
    plot1 <- plot1 + ggplot2::stat_ellipse(aes_string(x = PCnames[1], y = PCnames[2], colour = groupID))
  }
  return(plot1)
}
