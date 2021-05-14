##' @export

NMinfo <- function(data,info,as.fun){

    if(missing(as.fun)) as.fun <- NULL
    if(missing(info)) info <- NULL

    if(!is.null(info)){
        if(!info%in%c("details","columns","tables")){
            stop("If 'info' is supplied, it has to be one of 'details', 'columns', 'tables'.")
        }
    }
    
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    
    if(is.null(info)) {
        return(attributes(data)$meta)
    }
    
    as.fun(attributes(data)$meta[[info]])

}
