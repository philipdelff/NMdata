## do not export

writeNMinfo <- function(data,meta,append=FALSE,byRef=TRUE){

    
    if(append) {
        meta.0 <- meta
        meta <- NMinfo(data)
        meta <- meta[setdiff(names(meta),names(meta.0))]
        meta <- append(meta,meta.0)
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
