
##' Test if a variable can be interpreted by Nonmem
##' @description Nonmem can only interpret numeric data. However, a
##'     factor or a character variable may very well be interpretable
##'     by Nonmem (e.g. "33"). This function tells whether Nonmem will
##'     be able to read it.
##' @param x The vector to check Don't export
##' @return TRUE or FALSE
##' @export

NMisNumeric <- function(x){

### there is no is.POSIXct function available in base R. There is one in lubridate, but not to depend on that, we do a simple one here
    is.timestamp <- function(x){
        inherits(x, "POSIXct") ||
            inherits(x, "POSIXlt") ||
            inherits(x, "POSIXt")  ||
            inherits(x, "Date")
    }

    (!is.logical(x) &&
     !is.timestamp(x)) &&
        (is.numeric(x) ||
         suppressWarnings(!any(is.na(as.numeric(as.character(x)[!is.na(x)&!as.character(x)%in%c(".","NA")]))))
        )

}

