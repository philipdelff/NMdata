##' get a file or dir from a the NMdata package
##' @param ... parameters to pass to system.file
##' @details
##' a light wrapper around system.file
##' @family FileSystem
##' @export
NMdata_filepath <- function(...) {
  system.file(..., package = "NMdata")
}

##' Pretty wrapping of lines in NMdata vignettes
##' @param ... parameters to pass to strwrap
##' @param prefix Passed to strwrap. Default is " ". 
##' @param initial Passed to strwrap. Default is an empty string.
##' @param width Passed to strwrap. Default is 80.
messageWrap <- function(..., fun.msg=message, prefix = "\n", initial = "", width=85){
    fun.msg(strwrap(..., prefix = prefix, initial = initial, width=width))
}
