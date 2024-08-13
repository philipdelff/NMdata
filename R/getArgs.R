##' Get provided arguments as a named list
##' @param call Function call as provided by \code{sys.call()}.
##' @param env Environment in which to evaluate the arguments.
##' @return A named list of arguments and their values
##' @examples
##' afun <- function(){
##' NMdata:::getArgs(sys.call(),parent.frame())
##' }
##' afun()
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

