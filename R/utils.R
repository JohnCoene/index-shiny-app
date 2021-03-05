#' Package Data File
#' 
#' Retrieve the full path to a package data file.
#' 
#' @param ... File name.
#' 
#' @return A character vector of length one.
#' 
#' @keywords internal
pkg_data <- function(...){
  system.file("datasets", ..., package = "resilience.index")
}