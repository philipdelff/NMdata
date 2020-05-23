##' read csv data that is written to be read by nonmem
##' @param file The file to read. Must be pure text.
##' @param na.strings See read.table
##' @param header See read.table.
##' @param stringsAsFactors See read.table.
##' @param as.dt Return a data.table? data.table is default, if not a data.frame
##'     is returned.
##' @param ... passed to read.csv
##' @details This is just a shortcut to fread so you don't have to remember how
##'     to read the data that was exported for nonmem.
##' @importFrom data.table fread
##' @family Nonmem
##' @export

NMreadCsv <- function(file,na.strings=".",header=TRUE,stringsAsFactors=FALSE,as.dt=TRUE,...){
    dt <- fread(file=file,na.strings=na.strings,header=header,stringsAsFactors=stringsAsFactors,...)
    if(as.dt) return(dt) else return(as.data.frame(dt))
}
