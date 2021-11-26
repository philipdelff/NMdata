
##' Test if a variable can be interpreted by Nonmem
##' @description Nonmem can only interpret numeric data. However, a
##'     factor or a character variable may very well be interpretable
##'     by Nonmem (e.g. "33"). This function tells whether Nonmem will
##'     be able to read it.
##' @param x The vector to check Don't export
##' @param na.strings Tolerated strings that do not translate to
##'     numerics. Default is to accept "." because it's common to
##'     write missing values that way to Nonmem (even if Nonmem will
##'     handle them as zeros rather than missing). Notice actual NA's
##'     are accepted so you may want to use na.strings=NULL if you
##'     don't code missings as "." and just do this when writing the
##'     data set to a delimited file (like NMwriteData will do for
##'     you).
##' @return TRUE or FALSE
##' @export

NMisNumeric <- function(x,na.strings="."){
    
    
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
         suppressWarnings(!any(is.na(as.numeric(as.character(x)[!is.na(x)&!as.character(x)%in%na.strings]))))
        )

}

