##' @keywords internal

if(F){
    callArgs <- function(which=-1){
        
### args in call.
        ## no args are needed for this
        args.call <- as.list(
            match.call(
                definition = sys.function(which=which),
                call = sys.call(which=which)
            )
        )[-1]
        
### should this be envir=parent.frame(-which) ?
        

        argsconts <- lapply(args.call, eval, envir = parent.frame(n=-which+1))
        
        ## argsconts <- lapply(names(args.call), eval, envir = env)
        ## argsconts <- lapply(names(args.call), get, envir = env)
        ## unlist(sapply(args.call,as.character))
        ## argsconts <- lapply(args.call, function(x) get(as.character(x), envir = parent.frame(n=-which+1)))
        ## argsconts <- lapply(sapply(args.call,as.character) ,  get, envir = parent.frame(n=6))
        ## argsconts <- lapply(sapply(args.call,as.character) ,  get, envir = env)
        ##    digest(argsconts)

        as.list(do.call(c,argsconts))
    }
}


getArgs <- function(which=1)
{
    
    cl <- sys.call(-which)
    ## f <- get(as.character(cl[[1]]), mode="function", sys.frame(-which-1))
    ## accordng to Hadley, this is "better"
    f1 <- eval(cl[[1]], parent.frame(which))
    cl <- match.call(definition=f1, call=cl)
    as.list(cl)[-1]
}


##' Report if an argument is deprecated.
##'
##' Only supposed to be called from within a function. For now only
##' works for arguments that have been replaced by others.
##' 
##' @param oldarg The deprecated argument name (a character string).
##' @param newarg The non-deprecated argument name (a character string).
##' @return The coalesced value of arguments

##' @keywords internal
deprecatedArg <- function(oldarg,newarg,which=2){
    
    ## args <- callArgs(-2,env)
    args <- getArgs(which=which)
    names.args <- names(args)

    
    
### if oldarg and newarg are provided, stop
    if( oldarg %in% names.args &&
        newarg %in% names.args ) {
        stop(sprintf("%s is a deprecated argument. Just use %s and not %s.",oldarg,newarg,oldarg),call.=FALSE)
    }
    
### if only oldarg provided, return oldarg
    if( oldarg %in% names.args ) {
        message(sprintf("%s is a deprecated argument. Please use %s instead.",oldarg,newarg))
        return(args[[oldarg]])
    }
    
### return newarg
    parent.frame()[[newarg]]

}

