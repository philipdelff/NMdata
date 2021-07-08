##' Rename columns matching properties of data contents
##'
##' @description For instance, lowercase all columns that Nonmem
##'     cannot interpret (as numeric).
##' @param data data.frame in which to rename columns
##' @param fun.test Function that returns TRUE for columns to be
##'     renamed.
##' @param fun.rename Function that takes the existing column name and
##'     returns the new one.
##' @param invert.test Rename those where FALSE is returned from
##'     fun.test.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @return data with (some) new column names. Class as defined by
##'     as.fun.
##' @export

renameByContents <- function(data,fun.test,fun.rename,invert.test=FALSE,as.fun){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    value <- NULL
    variable <- NULL
    variable.new <- NULL
    N <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    dt <- as.data.table(data)
    dt.res <- dt[,lapply(.SD,fun.test)]
    dt.res.l <- melt(dt.res,measure.vars=colnames(dt.res))
    if(invert.test) dt.res.l[,value:=!value]
    dt.res.l[value==TRUE,variable.new:=fun.rename(variable)]
    ## duplicates in new names - only those that have been renamed.
    dups.renames <- dt.res.l[value==TRUE][,.N,by="variable.new"][N>1,variable.new]
    if(length(dups.renames)){
        stop("Multiple renames will result in the same column name(s): ",paste(dups.renames,sep=", "))
    }
    dt.res.l[value==FALSE,variable.new:=variable]

dups.renames <- dt.res.l[value==TRUE][,.N,by="variable.new"][N>1,variable.new]
    if(length(dups.renames)){
        stop("Renames will conflict with existing column name(s). Please rename these first: ",paste(dups.renames,sep=", "))
    }
    
    cols.to.rename <- dt.res.l[value==TRUE,as.character(variable)]
    setnames(dt,cols.to.rename,fun.rename)

    dt <- as.fun(dt)
    
    return(dt)
}
