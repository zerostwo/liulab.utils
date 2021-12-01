#' Download publication via Sci-Hub
#'
#' Use Sci-Hub to download publications via doi
#' @name scihub_dl
#'
#' @param doi The publication doi you want to download
#' @param scihub Available URLs of Sci-Hub
#' @param mode The mode with which to write the file. Useful values are "w",
#' "wb" (binary), "a" (append) and "ab". Not used for methods "wget" and
#' "curl". notably about using "wb" for Windows.
#' @param download Whether to download the PDF file of the publication
#'
#' @return PDF file URL address and PDF file
#' @export
#' @examples
#' \dontrun{
#' scihub_dl("10.1016/j.carbpol.2020.116446")
#' }
scihub_dl <- function(doi,
                      scihub = "sci-hub.se",
                      download = TRUE,
                      mode = "wb") {
  url <- paste0("https://", scihub, "/", doi)
  x <- readLines(url)
  i <- grep('id = "pdf"', x)
  pdf_url <- sub(".*(//.*\\.pdf).*", "https:\\1", x[i])

  if (download) {
    outfile <- sub(".*/", "", pdf_url)
    utils::download.file(pdf_url, destfile = outfile, mode = mode)
  }
  invisible(pdf_url)
}
