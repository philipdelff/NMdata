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
##' @details This is almost just a shortcut to fread so you don't have
##'     to remember how to read the data that was exported for
##'     Nonmem. The only added feature is that meta data as written by
##'     NMwriteData is read and attached as NMdata metadata before
##'     data is returned.
##' @return A data set of class as defined by as.fun.
##' @importFrom data.table fread
##' @family DataRead
##' @seealso NMwriteData
##' @export

NMreadCsv <- function(file,args.fread,as.fun=NULL,type=fnExtension(file),args.fst){

    
    if( !is.character(type) && length(type) == 1 ){
        stop("type must be a single character string.")
    }
    if(!type%in%cc(rds,fst)) type <- "text"
    
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    dt <- switch(type,
                 text={
                     if(missing(args.fread)) args.fread <- NULL
                     args.fread <- NMdataDecideOption("args.fread",args.fread)
                     do.call(fread,c(list(file=file),args.fread))
                 },
                 rds={
                     as.data.table(readRDS(file=file))
                 },
                 fst={
                     if (!requireNamespace("fst", quietly = TRUE)) {
                         stop("fst package could not be loaded. Either install fst or drop fst from formats.")
                     }
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
