##' get a file or dir from a the NMdata package
##' @param ... parameters to pass to system.file
##' @details
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
        list.args <- append(list.args,call.=FALSE)
    }
    do.call(fun.msg,args=list.args)
}


##' Determine as.fun to use based on argument and options
##' @param data Dataset to possibly convert.
##' @param as.fun A function to apply if not NULL.
##' @return Possibly converted data.
runAsFun <- function(data,as.fun){
    if(is.null(as.fun)){
        as.fun <- getOption("NMdata.as.fun")
    }
    if(is.null(as.fun)){
        return(data)
    }
    if(!is.function(as.fun)){
        stop("as.fun must be a function")
    }
    as.fun(data)
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
