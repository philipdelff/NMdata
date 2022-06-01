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
##' res <- NMscanMultiple(dir=system.file("examples/nonmem", package="NMdata"),file.pattern="xgxr.*\\.lst",as.fun="data.table")
##' res.mean <- res[,exp(mean(log(PRED))),by=.(model,NOMTIME)]
##' library(ggplot2)
##' ggplot(res.mean,aes(NMTIME,PRED,colour=model))+geom_line() 
##' @export


NMscanMultiple <- function(files,dir,file.pattern,as.fun,...){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

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


    
#### Section start: Code taken from NMwriteSection ####

    ## supply either file or file.pattern. dir only allowed if file.pattern
    if( missing(files) && missing(file.pattern) ){
        stop("You have to supply either file or file.pattern")
    }
    if(!missing(files)&& (!missing(file.pattern) || !missing(dir))){
        stop("If supplying files, file.pattern and dir cannot be used")
    }
    if(!missing(file.pattern)&&missing(dir)){
        stop("If using file.pattern, you have to supply dir too.")
    }
    
    if(!missing(files)&&length(files)>0){
        
        if(any(!file.exists(files))){
            if(!quiet){
                message("Files not found. Skipping:\n",paste(files[!file.exists(files)],collapse="\n"))
            }
        }
        all.files <- files[file.exists(files)]
    }
    
    
    if(!missing(file.pattern)){
        all.files <- list.files(path=dir,pattern=file.pattern,full.names=TRUE,recursive=FALSE)
    }
    
    if(length(all.files)==0){
        message("No existing files matched. Nothing to do.")
        return(invisible(NULL))
    }



###  Section end: Code taken from NMwriteSection
    
    if(length(all.files)==0) {
        cat("No files matched path/pattern criteria\n")
        return(NULL)
    }
    

    testfun <- function(x)NMscanData(x,as.fun="data.table",...)
    fun.apply <- function(x){
        cat(sprintf("\nReading %s:\n\n",x))
        res <- catchAnything(testfun)
        res
    }
    
    ## fun.apply <- function(x){
    ##     cat(sprintf("\nReading %s:\n\n",x))
    ##     res <- tryCatch(
    ##         expr={
    ##             NMscanData(x,as.fun="data.table",...)
    ##         }
    ##        ,error=function(e){"Failed"}
    ##        ,warning=function(w){w}
    ##     )
    ##     res
    ## }
    res.all.list <- lapply(all.files,catchAnything(testfun))

    ## list and count succesfull and unsuccesfull
    
    ## dt.lst <- data.table(lst=all.files,
    ##                      success=sapply(res.all.list,function(x)!any(class(x)=="try-error"))
    ##                      )

    dt.lst <- data.table(lst=all.files,
                         success=!hasError(res.all.list)
                         ,warning=hasWarning(res.all.list)
                         )

    ## add dimensions of the read data.
    names(res.all.list) <- all.files
    
    res.all.list <- lapply(res.all.list,function(x)x[[1]])
    dims.res <- dims(list.data=res.all.list[dt.lst[,which(success)]])
    dt.lst <- mergeCheck(dt.lst,dims.res,by.x="lst",by.y="data",all.x=T,quiet=TRUE)
    ##lapply(res.all.list[],NMinfo)
    
    info.list <- lapply(res.all.list,NMinfo)
    names(info.list) <- all.files
    
    res.all <- rbindlist(res.all.list[dt.lst[,which(success)]],fill=TRUE)
    writeNMinfo(res.all,info.list)
    
    setcolorder(dt.lst,cc(lst,nrows,ncols,success,warning))
    print(dt.lst)
    ## print(res.all[,.(.N),by=col.model])
    
    ## run as.fun
    res.all <- as.fun(res.all)
    setattr(res.all,"class",c("NMdata",class(res.all)))

    res.all

}
