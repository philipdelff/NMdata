##' @inheritParams compareCols
##' @export 

colLabels <- function(...){
    compareCols(...,fun.class=function(x)attributes(x)$label)
}
