NMcheckColnames <- function(file,as.fun,...){

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    ## translate and details are not allowed in ellipses
    dots <- list(...)
    if(any(c("translate","details")%in%dots)) messageWrap("translate and details arguments cannot be used with NMcheckColNames. Their values will be ignored.",fun.message=warning)
    
    data.input <- NMscanInput(file,translate=TRUE,details=TRUE)
   
    res <- as.fun(data.input$meta$colnames)
    return(res)

}
