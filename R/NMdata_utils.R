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
         suppressWarnings(!any(is.na(as.numeric(as.character(x)[!is.na(x)&!as.character(x)%in%c(".","NA")]))))
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
