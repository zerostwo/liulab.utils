#' Hello, world!
#'
#' This is an example function named 'hello'
#' which prints 'Hello, world!'.
#' You can learn more about package authoring with RStudio at:
#'     http://r-pkgs.had.co.nz/
#' Some useful keyboard shortcuts for package authoring:
#'    Install Package:           'Ctrl + Shift + B'
#'    Check Package:             'Ctrl + Shift + E'
#'    Test Package:              'Ctrl + Shift + T'
#'
#' @param string Any character, the default is "Hello, world!".
#'
#' @return Print out the string.
#' @export
#' @examples
#' hello("Good, job!")
hello <- function(string = "Hello, world!") {
  print(string)
}
