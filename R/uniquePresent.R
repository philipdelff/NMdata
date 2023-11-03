
uniquePresent <- function(x,req.n1=TRUE){
    un.x <- unique(x)
    un.x <- un.x[un.x!=""&!is.na(un.x)]
    if(length(un.x)==0) {
        warning("No value found")
        return(NA)
    }
    if(req.n1&&length(un.x)!=1) stop("Number of unique values is not 1.")
    un.x
}
