##' read csv data that is written to be read by nonmem
##' @param file The file to read. Must be pure text.
##' @param na.strings See read.table
##' @param header See read.table.
##' @param stringsAsFactors See read.table.
##' @param as.fun The default is to return data in data.tables. Pass a
##'     function in as.fun to convert to something else. If
##'     data.frames are wanted, use as.fun=as.data.frame. 
##' @param ... passed to read.csv
##' @details This is just a shortcut to fread so you don't have to remember how
##'     to read the data that was exported for nonmem.
##' @importFrom data.table fread
##' @family Nonmem
##' @export

NMreadCsv <- function(file,na.strings=".",header=TRUE,stringsAsFactors=FALSE,as.fun=NULL,...){

    as.fun <- getAsFun(as.fun)

    dt <- fread(file=file,na.strings=na.strings,header=header,stringsAsFactors=stringsAsFactors,...)

    if(!is.null(as.fun)) {
        dt <- as.fun(dt)
    }

    return(dt)
}
