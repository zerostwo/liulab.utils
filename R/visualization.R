#' Visualize the correlation of two genes
#'
#' @param expr Expression matrix, rows are genes and columns are cells
#' @param gene.x Genes displayed on the x-axis
#' @param gene.y Genes displayed on the y-axis
#' @param method a character string indicating which correlation coefficient is
#' to be used for the test. One of "pearson", "kendall", or "spearman",
#' can be abbreviated.
#' @return a list of ggplot objects
#' @importFrom ggplot2 element_blank element_text
#' @importFrom patchwork wrap_plots
#' @importFrom magrittr %>%
#' @importFrom stats cor.test
#' @export
DoCorPoint <- function(expr, gene.x, gene.y, method = "pearson") {
  sub.expr <- dplyr::as_tibble(t(as.matrix(expr))) %>%
    dplyr::select(gene.x, gene.y)
  colnames(sub.expr) <- c("gene.x", "gene.y")
  sub.expr <- as.data.frame(sub.expr)
  res <- cor.test(sub.expr$gene.x, sub.expr$gene.y, method = method)
  p.value <- res$p.value
  estimate <- res$estimate

  p <-
    ggplot2::ggplot(sub.expr, ggplot2::aes(x = gene.x, y = gene.y)) +
    ggplot2::geom_point(color="grey") +
    ggplot2::geom_smooth(
      method = "lm",
      formula = y ~ x,
      color = "#6b76ae",
      fill = "#cbc9e2"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      aspect.ratio = 1,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      axis.title = element_text(face = "italic", size = 8),
      plot.title = element_text(hjust = 0.5, size = 8),
      axis.text = element_text(size = 8, color = "black")
    ) +
    ggplot2::labs(
      x = gene.x,
      y = gene.y,
      title = paste0(
        "r = ",
        round(estimate, 2),
        ", p = ",
        format(p.value, digits = 2, scientific = T)
      )
    )
  return(p)
}
