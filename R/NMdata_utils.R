##' get a file or dir from a the NMdata package
##' @param ... parameters to pass to system.file
##' @details
##' a light wrapper around system.file
##' @export
NMdata_filepath <- function(...) {
  system.file(..., package = "NMdata")
}
