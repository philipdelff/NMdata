##' Run NMscanData on multiple models and stack results
##'
##' Useful function for meta analyses when multiple models are stored
##' in one folder and can be read with NMscanData using the same
##' arguments.
##'
##' @param files File paths to the models (control stream) to
##'     edit. See file.pattern too.
##' @param dir The directory in which to find the models. Passed to
##'     list.files(). See file.pattern argument too.
##' @param file.pattern The pattern used to match the filenames to read
##'     with NMscanData. Passed to list.files(). See dir argument too.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param ... Additional arguments passed to NMscanData.
##' @return All results stacked, class as defined by as.fun
##' @examples
##' res <- NMscanMultiple(dir=system.file("examples/nonmem", package="NMdata"),
##' file.pattern="xgxr01.*\\.lst",as.fun="data.table")
##' res.mean <- res[,.(meanPRED=exp(mean(log(PRED)))),by=.(model,NOMTIME)]
##' library(ggplot2)
##' ggplot(res.mean,aes(NOMTIME,meanPRED,colour=model))+geom_line() 
##' @export


NMscanMultiple <- function(files,dir,file.pattern,as.fun,...){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    lst <- NULL
    ncols <- NULL
    nrows <- NULL
    quiet <- NULL
    success <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

#### Section start: Define ad-hoc functions ####

    catchAnything <- function(fun)
        function(...) {
            warn <- err <- NULL
            res <- withCallingHandlers(
                tryCatch(fun(...), error=function(e) {
                    err <<- conditionMessage(e)
                    NULL
                }), warning=function(w) {
                    warn <<- append(warn, conditionMessage(w))
                    invokeRestart("muffleWarning")
                })
            list(res, warn=warn, err=err)
        }
    .has <- function(x, what)
        !sapply(lapply(x, "[[", what), is.null)
    hasWarning <- function(x) .has(x, "warn")
    hasError <- function(x) .has(x, "err")
    isClean <- function(x) !(hasError(x) | hasWarning(x))
    value <- function(x) sapply(x, "[[", 1)
    cleanv <- function(x) sapply(x[isClean(x)], "[[", 1)


### Section end: Define ad-hoc functions

    if(missing(files)) files <- NULL
    if(missing(dir)) dir <- NULL
    if(missing(file.pattern)) file.pattern <- NULL
    
    all.files <- getFilePaths(files=files,file.pattern=file.pattern,dir=dir)
    
    if(length(all.files)==0) {
        cat("No files matched path/pattern criteria\n")
        return(NULL)
    }
    

    testfun <- function(x) NMscanData(x,as.fun="data.table",...)
    fun.apply <- function(x){
        cat(sprintf("\nReading %s:\n\n",x))
        res <- catchAnything(testfun)
        res
    }
    
    res.all.list <- lapply(all.files,catchAnything(testfun))

    ## list and count succesfull and unsuccesfull
    dt.lst <- data.table(lst=all.files,
                         success=!hasError(res.all.list)
                        ,warning=hasWarning(res.all.list)
                         )

    ## add dimensions of the read data.
    names(res.all.list) <- all.files

    
    res.all.list <- lapply(res.all.list,function(x)x[[1]])
    if(dt.lst[,sum(as.numeric(success))] == 0 ) {
        stop("No models were succesfully read.")
    }
    dims.res <- dims(list.data=res.all.list[dt.lst[,which(success)]])
    dt.lst <- mergeCheck(dt.lst,dims.res,by.x="lst",by.y="data",all.x=T,quiet=TRUE)

    
    info.list <- lapply(res.all.list,NMinfo)
    names(info.list) <- all.files
    info.list <- append(info.list,list(all=dt.lst))
    
    res.all <- rbindlist(res.all.list[dt.lst[,which(success)]],fill=TRUE)
    writeNMinfo(res.all,info.list)
    
    setcolorder(dt.lst,cc(lst,nrows,ncols,success,warning))
    cat("\nOverview of model scanning results:\n")
    print(dt.lst)
    
    ## run as.fun
    res.all <- as.fun(res.all)
    setattr(res.all,"class",c("NMdata",class(res.all)))

    res.all

}
