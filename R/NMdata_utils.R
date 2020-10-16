##' get a file or dir from a the NMdata package
##' @param ... parameters to pass to system.file
##' @description
##' a light wrapper around system.file
##' @family FileSystem
##' @export
NMdata_filepath <- function(...) {
    system.file(..., package = "NMdata")
}

##' Pretty wrapping of lines in NMdata vignettes
##' @param ... parameters to pass to strwrap
##' @param fun.msg The function to pass the text through. Typically,
##'     message, warning, or stop.
##' @param prefix Passed to strwrap. Default is " ".
##' @param initial Passed to strwrap. Default is an empty string.
##' @param width Passed to strwrap. Default is 80.
messageWrap <- function(..., fun.msg=message, prefix = "\n", initial = "", width=80){

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
##'     classes. NMdata is mostly implemented in data.table, and the
##'     default for most functions is to return a data.table. However,
##'     most functions take an argument as.fun which controls this
##'     behaviour. If this argument is not set, the value of
##'     getOptions("NMdata.as.fun") decides (control using
##'     e.g. options(NMdata.as.fun=as.data.frame)). Notice, this is
##'     done before adding the NMdata class. So better use this
##'     functionality rather than converting the obtained dataset
##'     manually (which will drop the NMdata class)
##'
##' You can use the following values:
##' \itemize{
##'  \item{"none"}{Do nothing (i.e. keep data.table).}
##'  \item{a function}{Apply the function. You must supply the actual function, not a string representing it. Examples: as.data.frame or tibble::as_tibble. as.data.table won't have any effect since it will be applied to data.tables, so in this case, better use "none".}
##' \item{NULL}{Use default (i.e. try getOptions("NMdata.as.fun") and if still NULL, do nothing.)}
##' }
##' 
runAsFun <- function(data,as.fun){
    if(is.null(as.fun)){
        as.fun <- getOption("NMdata.as.fun")
    }
    if(is.character(as.fun)&&length(as.fun)==1&&as.fun=="none"){
        return(data)
    }
    if(is.null(as.fun)){
        return(data)
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
##' \item{set option}{If you use a function, you may want to set this as default behaviour. Say your output control stream is always called input.txt and located in the same dir as the output control stream, you can use (NMdata.file.mod=function(file) file.path(dirname(file),"input.txt"))}
##' }
##'
##' Notice, if the argument dir.data is used in NMscanData or
##' NMtransInput, the input control stream is not used at all.

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
    messageWrap("file.mod is not recognized as a function or a character",fun.msg=stop)
    
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
