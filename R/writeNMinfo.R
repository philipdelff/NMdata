##' Do the actual writing of meta data
##'
##' @param data A data set
##' @param meta The meta data to attach
##' @param append If FALSE, the existing meta data will be removed. If
##'     TRUE, metadata will be appended to existing metadata. However,
##'     this will not work recursively.
##' @param byRef Should always be TRUE.
##' @return The data with meta data attached
##' @keywords internal

## do not export

writeNMinfo <- function(data,meta,append=FALSE,byRef=TRUE){

    
    if(append) {
        
        meta.new <- meta
        meta.0 <- NMinfoDT(data)
        meta.0 <- meta.0[setdiff(names(meta.0),names(meta.new))]
        meta <- append(meta.0,meta.new)
    }

    if(byRef){
        setattr(data,"NMdata",
                as.list(meta)
                )
        return(invisible(data))
    } else {
        attr(data,"NMdata") <- meta
        return(data)
    }
    
}
