##' Extract column labels as defined in SAS
##' @param ... See `?compareCols`
##' @return A data.frame with variable and their labels
##' @seealso compareCols NMinfo 
##' @export 

colLabels <- function(...){
    compareCols(...,fun.class=function(x)attributes(x)$label)
}
