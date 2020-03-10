##' Get stamped info about object
##'
##' @param data The dataset to receive info about
##' @seealso stampObj
##' @family DataWrangling
##' @export

objInfo <- function(data){
    attr(data,"objInfo")
}
