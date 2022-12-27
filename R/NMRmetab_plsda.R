
NMRMetab_PLS_DA_plot <- function(data, groupID = "group", index_col = 3, components = c(1, 2), scale = F, elipses = F, size_point = 3) {
  compx <- compy <- group <- NULL

  if (scale == T) {
    data <- NMRMetab_norm_scale(data = data, index_col = index_col, scaling = "Pareto")
  }

  meta_data <- data[, 1:index_col - 1]
  matrix <- data[, index_col:ncol(data)]
  # scale the data

  model <- mixOmics::plsda(
    X = matrix,
    Y = meta_data[, groupID],
    ncomp = max(components), scale = F
  )

  comps_scores <- cbind.data.frame(model$variates$X[, components], "group" = meta_data[, groupID])

  if (length(components) == 3) {
    colnames(comps_scores) <- c("compx", "compy", "compz", "group")

    mycolors <- RColorBrewer::brewer.pal(n = 8, name = "Dark2")[1:length(unique(comps_scores$group))]
    comps_scores$color <- mycolors[as.numeric(as.factor(comps_scores$group))]

    p <- rgl::plot3d(
      x = comps_scores$compx, y = comps_scores$compy, z = comps_scores$compz,
      col = comps_scores$color,
      type = "s",
      size = size_point,
      radius = size_point,
      xlab = paste("comp", components[1], sep = ""),
      ylab = paste("comp", components[2], sep = ""),
      zlab = paste("comp", components[3], sep = ""),
      box = F
    )
  } else {
    colnames(comps_scores) <- c("compx", "compy", "group")

    p <- ggplot2::ggplot(comps_scores,
      aes(
        x = compx,
        y = compy,
        colour = group
      ),
      fill = "black"
    ) +
      ggplot2::geom_point(size = size_point) +
      ggplot2::theme_bw(base_size = 16) +
      ggplot2::scale_color_brewer(palette = "Dark2") +
      ggplot2::labs(
        title = "score plot",
        x = paste("comp", components[1], sep = ""),
        y = paste("comp", components[2], sep = "")
      ) +
      ggplot2::guides(fill = guide_legend(title = groupID))
    if (elipses == T) {
      p <- p + ggplot2::stat_ellipse(aes(x = compx, y = compy, colour = group))
    }
  }
  return(p)
}
