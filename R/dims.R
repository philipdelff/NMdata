## I don't think this should be exported. It's to be used by others -
## like compareCols - but the use of it is too limited for exporting.

dims <- function(...,list,keepNames=TRUE){

    if(missing(list)){
        dots <- list(...)
        if(keepNames){
            names.dots <- setdiff(as.character(match.call(expand.dots=T)),as.character(match.call(expand.dots=F)))
        }
    } else {
        dots <- list
        names.dots <- names(dots)
                    
    }
    if(!keepNames) {
        names.dots <- paste0("x",seq(ndots))
    }
    
    ndots <- length(dots)

    
    if(ndots<2) stop("At least two objects must be supplied")

    dt.dims <- rbindlist(lapply(1:ndots,function(n) data.table(data=names.dots[n],nrows=nrow(dots[[n]]),ncols=ncol(dots[[n]]))))

    dt.dims
    
}
