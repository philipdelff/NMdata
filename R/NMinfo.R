##' Get metadata from an NMdata object
##'
##' Extract metadata such as info on tables, columns and further
##' details in your favorite class
##'
##' @param data An object of class NMdata (a result of NMscanData)
##' @param info If not passed, all the metadata is returned. You can
##'     use "details", "tables", or "columns" to get only these
##'     subsets. If info is "tables" or "columns"
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @return A table of class as defined by as.fun in case info is
##'     "columns" or "tables". A list if info missing or equal to
##'     "details".
##' @export

NMinfo <- function(data,info,as.fun,nullEmpty=FALSE){


    ## if (!inherits(data, "NMdata")) {
    ##     ## stop("NMinfo is only intended for NMdata objects.")
    ##     meta <- attributes(data)$meta
    ##     if(is.null(meta)){
    ##         if(!nullEmpty){
    ##             warning("No meta data available. NULL returned.")
    ##         }
    ##     }
    ##     return(meta)
    ## }

    ## NMdata object

    if(missing(as.fun)) as.fun <- NULL
    if(missing(info)) info <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if (inherits(data, "NMdata")) {
    if(!is.null(info)){
        if(!info%in%c("details","columns","tables")){
            stop("For NMdata objects: If 'info' is supplied, it has to be one of 'details', 'columns', 'tables'.")
        }
    }
    }
    
    
    if(is.null(info)) {
        nms.meta <- names(attributes(data)$NMdata)
        return(setNames(lapply(nms.meta,function(x)NMinfo(data,info=x,as.fun=as.fun)),nms.meta))
    }

    out <- attributes(data)$NMdata[[info]]
    ##    if(info=="details") return(attributes(data)$meta$details)
    if(is.data.frame(out)) {
        out <- as.fun(out)
    }
    out
}
