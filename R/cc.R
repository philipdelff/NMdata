##' Create character vectors without quotation marks
##' 
##' When creating character vectors with several elements, it becomes
##' a lot of quotes to type. cc provides a simple way to skip the
##' quotes - but only for simple strings.
##' @param ... The unquoted names that will become character values in
##'     the returned vector.
##' @details Don't use cc with any special characters - only
##'     alphanumerics and no spaces supported.
##' @export

##' @examples
##' cc(a,b,`a b`)
##' cc(a,b,"a b")
##' ## be careful with spaces and special characters
##' cc( d)
##' cc(" d")
##' cc()

cc <- function(...){

    list.names <- as.list(match.call())[-1]
    
    out <- do.call(c,
            lapply(list.names, as.character)
            )

    if(is.null(out)) out <- character()
    out
}
