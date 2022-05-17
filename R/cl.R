##' Define a vector with factor levels in the same order as occurring in the vector.
##'
##' This is a shortcut for creating factors with levels as the order
##' of appearance of the specified levels.
##' 
##' @param ... unique elements or vectors with unique elements
##' @examples
##' factor("b","a")
##' cl("b","a")
##' x <- c("b","a")
##' factor(x)
##' cl(x)
##' @importFrom data.table setattr
##' @importFrom stats reorder
##' @seealso cc
##' @export

cl <- function(...){
    x <- c(...)

    ## x must be unique
    if(any(duplicated(x))) stop("Argument names (elements in resulting vector) must be unique.")

    y <- reorder(x,seq_along(x))
    
    setattr(y,"scores",NULL)
    y
    
}
