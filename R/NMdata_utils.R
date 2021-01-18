##' Pretty wrapping of lines in NMdata vignettes
##' @param ... parameters to pass to strwrap
##' @param fun.msg The function to pass the text through. Typically,
##'     message, warning, or stop. If NULL, nothing will happen, and
##'     NULL is invisibly returned.
##' @param prefix Passed to strwrap. Default is "\\n".
##' @param initial Passed to strwrap. Default is an empty string.
##' @param width Passed to strwrap. Default is 80.
messageWrap <- function(..., fun.msg=message, prefix = "\n", initial = "", width=80){

    if(is.null(fun.msg)) invisible(return(NULL))
    
    parent.call <- sys.call(sys.nframe() - 1L)

    list.args <- list(strwrap(paste("In",deparse(parent.call),":",...)
                             ,prefix = prefix
                             ,initial = initial
                             ,width=width)
                      )
    if("call."%in%names(formals(fun.msg))){
        list.args <- append(list.args,list(call.=FALSE))
    }
    do.call(fun.msg,args=list.args)
}


##' Determine as.fun to use based on argument and options
##' @param data Dataset to possibly convert.
##' @param as.fun A function to apply if not NULL.
##' @return Possibly converted data.
##' @details NMdata functions can return objects based on different
##'     classes. The general default is to return
##'     data.frames. However, if you prefer working with with other
##'     classes (typically data.table or tibble), you can have those
##'     returned instead. In general this is controlled by the run.as
##'     arguments to functions and the "NMdata.as.fun" option.
##'
##' You can use the following values for run.as (argument) and
##' NMdata.as.fun (option):
##' 
##' \itemize{
##' \item{"none"}{Return a data.table. Yes, a data.table and
##'  not a data.frame. If you work with data.table, this will give the
##'  best performance.}
##' \item{a function}{Apply the function. You
##'  must supply the actual function, not a string representing
##'  it. Examples: as.data.frame or tibble::as_tibble. Do not use
##'  as.data.table - use "none" to get a data.table.}
##' \item{NULL}{Use default, i.e. try getOptions("NMdata.as.fun") and if still NULL, rely on default behavior.}
##' }
##' 
##' It is perfectly possible but not recommended to convert the
##'     results coming out of NMdata functions for three reasons
##' 
##' \itemize{
##' \item{Convenience}{You would lose the NMdata class which enables a method like summary to work properly.}
##' \item{Reliability}{You would have to do it every time you have called an NMdata function increasing risk of errors.}
##' \item{Performance}{can be significantly better using "none".}
##' } 
##'
##' The functions that return table data structures (like a data.frame)
##'     take an argument as.fun which controls this behaviour. You can
##'     also control the default behaviour by the NMdata.as.fun
##'     option. If not NULL (the default value) as.fun always
##'     overrules getOption("NMdata.run.as").
##'
##' For functions that read and process data from the file system
##' (like NMreadCsv, NMscanData, NMscanTables and many others),
##' default is data.frame. Default can be overruled using
##' options(NMdata.run.as=...), and the default behaviour can be
##' overruled using the run.as argument.
##' 
##' For functions that process a data set supplied in an argument (as
##'     opposed to reading data from the file system), the class of
##'     the input data makes a difference. This applies to functions
##'     like mergeCheck, NMorderCols, findCovs, findVars, flagsAssign,
##'     flagsCount. If a data.table is supplied to these (and as.fun
##'     is NULL), a data.table will be returned. If not, a data.frame
##'     is still default, and the default can be configured using
##'     options(NMdata.run.as=...).
##'
##' NMdata heavily uses data.table under the hood, so "none" means
##' that no conversion is done by the end of the function. Since
##' NMdata has no other dependencies than data.table, it cannot
##' convert to other classes unless the user supply the conversion
##' function. 
##'
##' @examples
##' dat <- NMscanData(system.file("examples/nonmem/xgxr001.lst",package="NMdata"))
##' class(dat)
##' dat <- NMscanData(system.file("examples/nonmem/xgxr001.lst",package="NMdata"),
##'                   as.fun="none")
##' class(dat)
##' options(NMdata.run.as="none")
##' dat <- NMscanData(system.file("examples/nonmem/xgxr001.lst",package="NMdata"))
##' class(dat)
##' library(tibble)
##' dat <- NMscanData(system.file("examples/nonmem/xgxr001.lst",package="NMdata"),
##'                   as.fun=tibble::as_tibble)
##' class(dat)


runAsFun <- function(data,as.fun){
    if(is.null(as.fun)){
        as.fun <- getOption("NMdata.as.fun")
    }
    if(is.character(as.fun)&&length(as.fun)==1&&as.fun=="none"){
        return(data)
    }
    if(is.null(as.fun)){
        ## default behaviour. Function or "none" not specified in
        ## argument, nor in option.
        ## return(data)
        as.fun <- as.data.frame
    }
    if(!is.function(as.fun)){
        stop("as.fun must be a function or the character string \"none\".")
    }
    as.fun(data)
}

##' Determine file.mod to use based on argument and options
##' @param file.lst Path to output control stream.
##' @param file.mod Path or function. Default is NULL. See details. 
##' @return path to .mod file
##' @details NMdata needs the input control stream to find the path to
##'     the input data file. You have a few different options.
##'     \itemize{
##' \item{PSN style}{By default, NMdata assumes that
##'     by stripping the extension from the output control stream and
##'     appending .mod, it will find the input control stream.}
##' \item{path}{file.mod="path/to/input/control/strem"}
##' \item{translation function}{file.path can be a function that takes the lst path as argument and returns the input control stream path.}
##' \item{set option}{If you use a function, you may want to set this as default behaviour. Say your output control stream is always called input.txt and located in the same dir as the output control stream, you can use
##' options(NMdata.file.mod=function(file) file.path(dirname(file),"input.txt"))
##' }
##' }
##'
##' Notice, if the argument dir.data is used in NMscanData or
##' NMtscanInput, the input control stream is not used at all.

getFileMod <- function(file.lst,file.mod=NULL){
    
    if(is.null(file.mod)){
        file.mod <- getOption("NMdata.file.mod")
        if(!is.null(file.mod) && !is.function(file.mod)) {
            messageWrap("When file.mod is specified by getOption(\"NMdata.file.mod\"), it has to be a function.",
                        fun.msg=stop)
        }
    }
    if(is.null(file.mod)){
        return(sub("\\.lst","\\.mod",file.lst))
    }
    if(is.function(file.mod)) {
        return(file.mod(file.lst))
    }
    if(is.character(file.mod)){
        return(file.mod)
    }
    messageWrap("file.mod is not recognized as a function or a character",
                fun.msg=stop)
    
}



##' Test if a variable can be interpreted by Nonmem
##' @description Nonmem can only interpret numeric data. However, a
##'     factor or a character variable may very well be interpretable
##'     by Nonmem (e.g. "33"). This function tells whether Nonmem will
##'     be able to read it.
##' @param x The vector to check Don't export

## Don't export

NMisNumeric <- function(x){

### there is no is.POSIXct function available in base R. There is one in lubridate, but not to depend on that, we do a simple one here
    is.timestamp <- function(x){
        inherits(x, "POSIXct") ||
            inherits(x, "POSIXlt") ||
            inherits(x, "POSIXt")  ||
            inherits(x, "Date")
    }

    (!is.logical(x) &&
     !is.timestamp(x)) &&
        (is.numeric(x) ||
         suppressWarnings(!any(is.na(as.numeric(as.character(x)[!as.character(x)%in%c(".","NA")]))))
        )

}


is.NMdata <- function(x){
    inherits(x,"NMdata")
}

merge.NMdata <- function(x,...){
    setattr(x,"class",setdiff(class(x),"NMdata"))
    merge(x,...)
}

t.NMdata <- function(x,...){
    setattr(x,"class",setdiff(class(x),"NMdata"))
    t(x,...)
}

dimnames.NMdata <- function(x,...){
    setattr(x,"class",setdiff(class(x),"NMdata"))
    dimnames(x,...)
}

rbind.NMdata <- function(x,...){
    setattr(x,"class",setdiff(class(x),"NMdata"))
    rbind(x,...)
}

cbind.NMdata <- function(x,...){
    setattr(x,"class",setdiff(class(x),"NMdata"))
    cbind(x,...)
}

unNMdata <- function(x){
    setattr(x,"class",setdiff(class(x),"NMdata"))
    setattr(x,"meta",NULL)
}
