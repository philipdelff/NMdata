##' Pretty wrapping of lines in NMdata vignettes
##' @param ... parameters to pass to strwrap
##' @param fun.msg The function to pass the text through. Typically,
##'     message, warning, or stop. If NULL, nothing will happen, and
##'     NULL is invisibly returned.
##' @param prefix Passed to strwrap. Default is "\\n".
##' @param initial Passed to strwrap. Default is an empty string.
##' @param width Passed to strwrap. Default is 80.
##' @param track.msg If TRUE, the name of the function throwing the
##'     message/warning/error is mentioned. This is not default but
##'     useful when using function inside other functions.
##' @return Nothing.
##' @keywords internal

### Internal function. Do not export.

messageWrap <- function(..., fun.msg=message, prefix = "\n", initial = "", width,track.msg=FALSE){
    
    if(missing(width)) width=options("width")[["width"]]
    if(is.null(fun.msg)) invisible(return(NULL))
    
    parent.call <- sys.call(sys.nframe() - 1L)

    msg0 <- ""
    if(track.msg) msg0 <- paste("In",paste(deparse(parent.call),collapse=" "),":")
    list.args <- list(strwrap(paste0(msg0,...)
                             ,prefix = prefix
                             ,initial = initial
                             ,width=width)
                      )
    if("call."%in%names(formals(fun.msg))){
        list.args <- append(list.args,list(call.=FALSE))
    }
    do.call(fun.msg,args=list.args)
}
