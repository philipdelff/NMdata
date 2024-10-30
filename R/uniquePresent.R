##' Extract unique non-missing value from vector
##'
##' @param x A vector, either numeric or character.
##' @param req.n1 Require one unique value? If `TRUE` (default), an
##'     error is thrown if non-unique values found. If `FALSE`, all
##'     the unique values are returned.
##' @param na.pattern In addition to NA-elements, what text strings
##'     should be considered missing? Default is empty strings and
##'     strings only containing white spaces (`na.pattern="^ *$"`).
##' @details This function is particularly useful when combining data
##'     sets of which only some contain certain
##'     variables. \code{uniquePresent} with `req.n1=TRUE` makes sure the
##'     result is a single unique value (e.g., within subjects). A
##'     typical use is carrying subject-level covariates from one data
##'     set to another in a longitudinal analysis.
##' @return a vector of same class as `x`
##' @export
uniquePresent <- function(x,req.n1=TRUE,na.pattern){
    if(missing(na.pattern)) na.pattern <- "^ *$"
    
    un.x <- unique(x)
    un.x <- un.x[!is.na(un.x) &  !grepl(pattern=na.pattern,un.x) ]
    
    if(length(un.x)==0) {
        warning("No value found")
        return(NA)
    }
    if(req.n1&&length(un.x)!=1) stop("Number of unique values is not 1.")

    return(un.x)
}
