##' Report if an argument is deprecated.
##'
##' Only supposed to be called from within a function. For now only
##' works for arguments that have been replaced by others.
##' 
##' @param oldarg The deprecated argument name (a character string).
##' @param newarg The non-deprecated argument name (a character
##'     string).
##' @param args List of arguments in the function call to look for
##'     oldarg and newarg. See `?getArgs`. If missing, `getArgs()`
##'     will be called from within `deprecatedArg`. See `which` too.
##' @param which If calling `getArgs` this is passed along, referring
##'     to how many environments to jump to look at arguments.
##' @return The coalesced value of arguments
##' @family arguments
##' @keywords internal
##' @examples
##' \dontrun{
##'    fun1 <- function(a=1,b=2){
##'        ## b is deprecated
##'        a <- deprecatedArg("b","a")
##'        a
##'    }
##'
##'    expect_error(
##'        fun1(a=1,b=2)
##'    )
##'    expect_message(
##'        fun1(b=2)
##'    )
##'}

deprecatedArg <- function(oldarg,newarg,args,msg=NULL,which=2){
    
    ## args <- callArgs(-2,env)
    if(missing(args) || is.null(args)){
        args <- getArgs(which=which)
    }
    names.args <- names(args)

    
### if there is no newarg, we just check if oldarg is used and if so
### report that it's deprecated.
    
    if( missing(newarg) ){
        if( oldarg %in% names.args ) {
            message(sprintf(paste("%s is a deprecated argument.",msg),oldarg))
            return(invisible(args[[oldarg]]))
        }
        return(invisible(NULL))
    }
    
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

