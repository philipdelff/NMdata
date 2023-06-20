##' Get provided arguments as a named list
##' @param which How many environment levels to go up to look for
##'     arguments.
##' @return A named list
##' @family arguments
##' @keywords internal

## getArgs <- function(which=1){

##     cl <- sys.call(-which)
##     ## f1 <- get(as.character(cl[[1]]), mode="function", sys.frame(-which-1))
##     ## accordng to Hadley, this is "better"
##     ls.par <- ls(pos=parent.frame(n=1))

##     f1 <- eval(cl[[1]], parent.frame(which))
##     cl <- match.call(definition=f1, call=cl,envir=parent.frame(which+1))
##     as.list(cl)[-1]
## }


getArgs <- function(which = 1) {
    cl <- sys.call(-which)
    
    f1 <- eval(cl[[1]], parent.frame(which))
    ##  cl <- match.call(definition = f1, call = cl)
    cl <- match.call(definition=f1, call=cl,envir=parent.frame(which+1))
    cl[[1]] <- quote(list)
    ##eval(cl, parent.frame(which))
    eval(cl, parent.frame(which+1))
}

