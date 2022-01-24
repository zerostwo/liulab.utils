#' Calculate the correlation of two features
#'
#' @param adata Expression matrix, rows are cells and columns are features
#' @param feature.x first feature
#' @param feature.y second feature
#' @param cutoff.x Threshold for feature.x value
#' @param cutoff.y Threshold for feature.y value
#' @param method a character string indicating which correlation coefficient is
#' to be used for the test. One of "pearson", "kendall", or "spearman",
#' can be abbreviated.
#' @return a tibble objects
#' @importFrom magrittr %>%
#' @importFrom stats cor.test
#' @export
calculate_correlation <-
  function(adata,
           feature.x,
           feature.y,
           cutoff.x = -Inf,
           cutoff.y = -Inf,
           method = "pearson") {
    sub.expr <- adata[, c(feature.x, feature.y)]
    sub.expr <-
      sub.expr[sub.expr[, feature.x] > cutoff.x &
                 sub.expr[, feature.y] > cutoff.y, ]
    if (!is.null(nrow(sub.expr))) {
      if (nrow(sub.expr) >= 3) {
        cor.test.res <-
          cor.test(sub.expr[, feature.x],
                   sub.expr[, feature.y],
                   method = method)
        p.value <- cor.test.res$p.value
        estimate <- cor.test.res$estimate
        return(
          dplyr::tibble(
            feature_x = feature.x,
            feature_y = feature.y,
            p_value = p.value,
            estimate = estimate,
            num = nrow(sub.expr)
          )
        )
      }
    }
  }
