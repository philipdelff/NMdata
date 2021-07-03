##' Compare $DATA to column names in input data
##'
##' Misspecification of column names in $DATA are a common source of
##' problems with Nonmem models, and one of the first things to check
##' when seemingly inexplainable things happen. This function lines up
##' input data column names with $DATA and how NMscanData will
##' interpret $DATA so you can easily spot if something is off.
##'
##' @param file A Nonmem control stream or list file
##' @param as.fun See ?NMdataConf
##' @param ... Additional arguments passed to
##' @return An overview of input column names and how they are translated
##' @export 

NMcheckColnames <- function(file,as.fun,...){

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    ## translate and details are not allowed in ellipses
    dots <- list(...)
    if(any(c("translate","details")%in%dots)) messageWrap("translate and details arguments cannot be used with NMcheckColNames. Their values will be ignored.",fun.message=warning)

    dots$translate <- TRUE
    dots$details <- TRUE
    dots$file <- file

    data.input <- do.call(NMscanInput,dots)
   
    res <- NMinfo(data.input,"input.colnames",as.fun=as.fun)
    return(res)

}
