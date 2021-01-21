##' Get stamped info about object
##'
##' Very simple function that extracts info stored by stampObj.
##'
##' @param data The dataset to receive info about
##' @seealso stampObj
##' @family DataCreate
##' @export

objInfo <- function(data){
    attr(data,"objInfo")
}
