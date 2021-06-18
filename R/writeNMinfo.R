## do not export

writeNMinfo <- function(data,meta,byRef=TRUE){
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
