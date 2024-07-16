##' Get provided arguments as a named list
##' @param which How many environment levels to go up to look for
##'     arguments.
##' @return A named list
##' @family arguments
##' @keywords internal

getArgs <- function(call,env) {
    ##cl <- sys.call(-which)
    
    f1 <- eval(call[[1]], env)
    ##  cl <- match.call(definition = f1, call = cl)
    cl <- match.call(definition=f1, call=call,envir=env)
    cl[[1]] <- quote(list)
    ##eval(cl, parent.frame(which))
    eval(cl, env)
}

