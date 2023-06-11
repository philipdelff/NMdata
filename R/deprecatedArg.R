##' @keywords internal

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
    argsconts <- lapply(args.call, eval, envir = parent.frame(n=-which))
    ##    digest(argsconts)

    do.call(c,as.list(argsconts))
}

##' @keywords internal
deprecatedArg <- function(oldarg,newarg){

    args <- callArgs(-2)
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

