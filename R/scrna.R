#' SciBet: Single Cell Identifier Based on Entropy Test
#'
#' @param object 	An Seurat object
#' @param model.path A csv format file, pre-trained prediction model path
#' @param assay Assay to use in cell annotation
#' @param slot Slot to pull data from
#'
#' @return The vector contains the barcode of each cell and predicted cell type
#' @export
#' @examples
#' \dontrun{
#' # Import Seurat objects
#' Load("pbmc_small")
#' # Cell type prediction
#' label <- scibet(seurat.obj, "/public/major_human_cell_types.csv")
#' # Add the prediction result to the Seurat object
#' seurat.obj$label <- label
#' }
scibet <-
  function(object,
           model.path,
           assay = "RNA",
           slot = "data") {
    expr <- Seurat::GetAssayData(object,
                                 slot = slot,
                                 assay = assay)
    expr <- t(as.matrix(expr))
    expr <- dplyr::as_tibble(expr)
    expr$label <- colnames(object)
    print(expr[1:4, 1:4])

    model <-
      readr::read_csv(model.path)
    model <- scibet::pro.core(model)

    ori_label <- expr$label
    expr <- expr[, -ncol(expr)]

    prd <- scibet::LoadModel(model)
    label <- scibet::prd(expr)
    return(label)
  }

#' Convert tpm to count
#'
#' @param tpm TPM expression
#'
#' @return Count expression
#' @export
tpm2count <- function(tpm) {
  counts <-
    apply(
      tpm,
      MARGIN = 2,
      FUN = function(x)
        x = x / sort(x[x > 0])[1]
    )
  return(counts)
}
# https://github.com/satijalab/seurat/issues/4686
