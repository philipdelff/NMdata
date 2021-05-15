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

NMinfo <- function(data,info,as.fun){
    if (!inherits(data, "NMdata")) 
        stop("NMinfo is only intended for NMdata objects.")
    
    if(missing(as.fun)) as.fun <- NULL
    if(missing(info)) info <- NULL

    if(!is.null(info)){
        if(!info%in%c("details","columns","tables")){
            stop("If 'info' is supplied, it has to be one of 'details', 'columns', 'tables'.")
        }
    }
    
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    
    if(is.null(info)) {
        nms.meta <- names(attributes(data)$meta)
        return(setNames(lapply(nms.meta,function(x)NMinfo(data,info=x,as.fun=as.fun)),nms.meta))
    }

    if(info=="details") return(attributes(data)$meta$details)
    as.fun(attributes(data)$meta[[info]])

}
