##' generate a name for a new data column that is not already in use.
##' @param data The dataset to find a new element name for
##' @param names Character vector of names that must not be
##'     matched. Only one of data and names can be supplied.
##' @param base The base name of the new element. A number will
##'     appended to this string that will ensure that the new element
##'     name is not already in use.
##' @param i1 Where to start the search for a smallest available index
##'     number to add to `base` if necessary.
##' @param max.it Maximum number of iterations on element name.
##' @param prefer.plain If base isn't in use already, use it without a
##'     digit appended?
##' @return A character string
##' @seealso make.names
##' @family DataCreate
##' @keywords internal
##' @export

tmpcol <- function(data,names=NULL,base="tmpcol",i1=1,max.it=100,prefer.plain=TRUE){
    stopifnot(xor(missing(data),is.null(names)))

    if(!missing(data)) names <- names(data)

    colname <- NULL
    if(prefer.plain){
        if(!base%in%names) colname <- base
    }

    if(is.null(colname)){
        i <- i1
        while(paste0(base,i)%in%names) {
            i  <- i+1
            if(i>max.it) stop("Maximum number of iterations reached. Check if something is wrong, and maybe increase max.it.") 
        }
        colname <- paste0(base,i)
    }

    return(colname)
}
