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

    list.args <- list(strwrap(paste("In",paste(deparse(parent.call),collapse=" "),":",...)
                             ,prefix = prefix
                             ,initial = initial
                             ,width=width)
                      )
    if("call."%in%names(formals(fun.msg))){
        list.args <- append(list.args,list(call.=FALSE))
    }
    do.call(fun.msg,args=list.args)
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
