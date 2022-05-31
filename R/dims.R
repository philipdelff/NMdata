##' Get dimensions of multiple objects
##' @param ... data sets
##' @param list.data As alternative to ..., you can supply the data
##'     sets in a list here.
##' @param keep.names If TRUE, the original dataset names are used in
##'     reported table. If not, generic x1, x2,... are used. The
##'     latter may be preferred for readability in some cases.
##' @param as.fun A function that will be run on the result before
##'     returning. If first input data set is a data.table, the
##'     default is to return a data.table, if not the default is to
##'     return a data.frame. Use whatever to get what fits in with
##'     your workflow. Default can be configured with NMdataConf.
##' @param keepNames Deprecated. Use keep.names instead.
##' @return A data.frame with dimensions of objects in ... Actual
##'     class defined by as.fun.
##' @family DataWrangling
##' @export


dims <- function(...,list.data,keep.names=TRUE,as.fun=NULL,keepNames){

    if(!missing(keepNames)){
        message("compareCols: keepNames argument is deprecated. Use keep.names instead. For now, keepNames is overruling keep.names.")
        keep.names <- keepNames
    }

    if(missing(list.data)){
        dots <- list(...)
        if(keep.names){
            names.dots <- setdiff(as.character(match.call(expand.dots=TRUE)),as.character(match.call(expand.dots=FALSE)))
        }
    } else {
        dots <- list.data
        names.dots <- names(dots)
        
    }
    if(!all(sapply(dots,is.data.frame))) stop("All objects must be data.frames.")
    if(!keep.names) {
        names.dots <- paste0("x",seq(ndots))
    }

    if(is.data.table(dots[[1]]) && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    ndots <- length(dots)

    dt.dims <- rbindlist(lapply(1:ndots,function(n) data.table(data=names.dots[n],nrows=nrow(dots[[n]]),ncols=ncol(dots[[n]]))))

    as.fun(dt.dims)
    
}
