##' read csv data that is written to be read by nonmem
##'
##' This function is especially useful if the csv file was written
##' using NMwriteData.
##' 
##' @param file The file to read. Must be pure text.
##' @param args.fread List of arguments passed to fread. Notice that
##'     except for "file", you need to supply all
##'     arguments to fread if you use this argument. Default values
##'     can be configured using NMdataConf.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @details This is just a shortcut to fread so you don't have to
##'     remember how to read the data that was exported for nonmem.
##' @importFrom data.table fread
##' @family DataRead
##' @seealso NMwriteData
##' @export

NMreadCsv <- function(file,args.fread,as.fun=NULL){

    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(args.fread)) args.fread <- NULL
    args.fread <- NMdataDecideOption("args.fread",args.fread)
    
    dt <- do.call(fread,c(list(file=file),args.fread))

    dt <- as.fun(dt)
    file.csv.meta <- paste0(fnExtension(file,ext=""),"_meta.txt")
    if(file.exists(file.csv.meta)){
        meta <- fread(file.csv.meta,sep=",",header=TRUE)
        meta.list <- setNames(as.list(meta$value),meta$parameter)
        attr(dt,"objInfo") <- meta.list
    }
    
    return(dt)
}
