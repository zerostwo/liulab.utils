#' Visualize the correlation of two features
#'
#' @param adata Expression matrix, rows are cells and columns are features
#' @param feature.x features displayed on the x-axis
#' @param feature.y features displayed on the y-axis
#' @param cutoff.x Threshold for feature.x value
#' @param cutoff.y Threshold for feature.y value
#' @param method a character string indicating which correlation coefficient is
#' to be used for the test. One of "pearson", "kendall", or "spearman",
#' can be abbreviated.
#' @return a list of ggplot objects
#' @importFrom magrittr %>%
#' @importFrom stats cor.test
#' @export
plot_correlation <-
  function(adata,
           feature.x,
           feature.y,
           cutoff.x = -Inf,
           cutoff.y = -Inf,
           method = "pearson") {
    sub.expr <- adata[, c(feature.x, feature.y)]
    sub.expr <-
      sub.expr[sub.expr[, feature.x] > cutoff.x &
                 sub.expr[, feature.y] >
                 cutoff.y,]
    cor.res <-
      cor.test(sub.expr[, feature.x], sub.expr[, feature.y], method = method)
    p.value <- cor.res$p.value
    estimate <- cor.res$estimate
    colnames(sub.expr) <- c("feature.x", "feature.y")
    sub.expr %>% as.data.frame() %>%
      ggplot2::ggplot(ggplot2::aes(x = feature.x, y = feature.y)) +
      ggrastr::rasterise(ggplot2::geom_point(color = "grey"), dpi = 600) +
      ggplot2::geom_smooth(
        method = "lm",
        formula = y ~ x,
        color = "#6b76ae",
        fill = "#cbc9e2"
      ) +
      ggplot2::theme_test() +
      ggplot2::theme(
        aspect.ratio = 1,
        axis.title = element_text(face = "italic", size = 8),
        plot.title = element_text(hjust = 0.5, size = 8),
        axis.text = element_text(size = 8, color = "black")
      ) +
      ggplot2::labs(
        x = feature.x,
        y = feature.y,
        title = paste0(
          "r = ",
          round(estimate, 2),
          ", p = ",
          format(p.value, digits = 2, scientific = T)
        )
      )
  }
