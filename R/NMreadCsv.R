##' read csv data that is written to be read by nonmem
##'
##' This function is especially useful if the csv file was written
##' using NMwriteData.
##' 
##' @param file The file to read. Must be pure text.
##' @param na.strings See data.table::fread.
##' @param header See data.table::fread.
##' @param as.fun The default is to return data as a data.frame. Pass a function
##'     (say tibble::as_tibble) in as.fun to convert to something else. If
##'     data.tables are wanted, use as.fun="data.table". The default can be
##'     configured using NMdataConf.
##' @param ... passed to fread.
##' @details This is just a shortcut to fread so you don't have to remember how
##'     to read the data that was exported for nonmem.
##' @importFrom data.table fread
##' @family DataRead
##' @seealso NMwriteData
##' @export

NMreadCsv <- function(file,na.strings=".",header=TRUE,as.fun=NULL,...){

    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    dt <- fread(file=file,na.strings=na.strings,header=header,...)

    dt <- as.fun(dt)

    return(dt)
}
