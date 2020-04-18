##' read csv data that is written to be read by nonmem
##' @param file The file to read. Must be pure text.
##' @param na.strings See read.table
##' @param header See read.table.
##' @param stringsAsFactors See read.table.
##' @param ... passed to read.csv
##' @details This is just a shortcut to read.csv so you don't have to remember
##'     how to read the data that was exported for nonmem.
##' @importFrom data.table fread
##' @family Nonmem
##' @export
NMreadCsv <- function(file,na.strings=".",header=TRUE,stringsAsFactors=FALSE,as.dt=TRUE,...){
    ## read.csv(file=file,na.strings=na.strings,header=header,stringsAsFactors=stringsAsFactors,...)
    dt <- fread(file=file,na.strings=na.strings,header=header,stringsAsFactors=stringsAsFactors,...)
    if(as.dt) return(dt) else return(as.data.frame(dt))
}
