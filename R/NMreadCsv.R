##' Read input data formatted for Nonmem
##'
##' This function is especially useful if the csv file was written
##' using NMwriteData.
##' 
##' @param file The file to read. Must be pure text.
##' @param args.fread List of arguments passed to fread. Notice that
##'     except for "file", you need to supply all arguments to fread
##'     if you use this argument. Default values can be configured
##'     using NMdataConf.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param format Format of file to read. Must be one of "csv", "rds",
##'     or "fst". Default is to determine this from the file name
##'     extension. Notice, if a delimited format is used, the
##'     extension can very well be different from "csv" (say file name
##'     is "input.tab"), and one must provide format="csv". This will
##'     work for any delimited format supported by fread and does not
##'     need to be comma-separated even though the format name
##'     indicates so.
##' @param args.fst Optional arguments to pass to \code{read_fst} if
##'     code{format="fst"} is used.
##' @details This is almost just a shortcut to fread so you don't have
##'     to remember how to read the data that was exported for
##'     Nonmem. The only added feature is that meta data as written by
##'     NMwriteData is read and attached as NMdata metadata before
##'     data is returned.
##' @return A data set of class as defined by as.fun.
##' @importFrom data.table fread
##' @importFrom fst read_fst
##' @family DataRead
##' @seealso NMwriteData
##' @export

NMreadCsv <- function(file,args.fread,as.fun=NULL,format=fnExtension(file),args.fst){

    
    if( !is.character(format) && length(format) == 1 ){
        stop("format must be a single character string.")
    }
    if(!format%in%c("rds","fst")) format <- "text"
    
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    dt <- switch(format,
                 text={
                     if(missing(args.fread)) args.fread <- NULL
                     args.fread <- NMdataDecideOption("args.fread",args.fread)
                     do.call(fread,c(list(file=file),args.fread))
                 },
                 rds={
                     as.data.table(readRDS(file=file))
                 },
                 fst={
                     ## if (!requireNamespace("fst", quietly = TRUE)) {
                     ##     stop("fst package could not be loaded. Either install fst or drop fst from formats.")
                     ## }
                     if(missing(args.fst)){
                         args.fst <- list(as.data.table=TRUE)
                     }
                     do.call(read_fst,c(list(path=file),args.fst))
                 }
                 )
    
    dt <- as.fun(dt)
    file.csv.meta <- fnAppend(fnExtension(file,ext="txt"),"meta")
    
    if(file.exists(file.csv.meta)){
        
        meta <- fread(file.csv.meta,sep=",",header=TRUE)
        meta.list <- setNames(as.list(meta$value),meta$parameter)
        writeNMinfo(dt,list(dataCreate=meta.list))
    }
    
    return(dt)
}
