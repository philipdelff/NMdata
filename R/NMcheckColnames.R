##' Compare $INPUT in control stream to column names in input data
##'
##' Mis-specification of column names in $DATA is a common source of
##' problems with Nonmem models, and should be one of the first things
##' to check for when seemingly inexplicable things happen. This
##' function lines up input data column names with $DATA and how
##' NMscanData will interpret $DATA so you can easily spot if
##' something is off.
##'
##' @param file A Nonmem control stream or list file
##' @param as.fun See ?NMdataConf
##' @param ... Additional arguments passed to
##' @return An overview of input column names and how they are translated
##' @family debug
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
