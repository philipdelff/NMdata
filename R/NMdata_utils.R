##' Remove NMdata class and discard NMdata meta data
##' @param x An 'NMdata' object.
##' @return x stripped from the 'NMdata' class
##' @export
unNMdata <- function(x){
    setattr(x,"NMdata",NULL)
    setattr(x,"class",setdiff(class(x),"NMdata"))
}


##' Check if an object is 'NMdata'
##' @param x Any object
##' @return logical if x is an 'NMdata' object
##' @export
is.NMdata <- function(x){
    inherits(x,"NMdata")
}


##' Basic arithmetic on NMdata objects
##'
##' @param x an NMdata object
##' @param ... arguments passed to other methods.
##' @details When 'dimnames', 'merge', 'cbind', 'rbind', or 't' is
##'     called on an 'NMdata' object, the 'NMdata' class is dropped,
##'     and then the operation is performed. So if and 'NMdata' object
##'     inherits from 'data.frame' and no other classes (which is
##'     default), these operations will be performed using the
##'     'data.frame' methods. But for example, if you use 'as.fun' to
##'     get a 'data.table' or 'tbl', their respective methods are used
##'     instead.
##' @return An object that is not of class 'NMdata'.
##' @name NMdataOperations
NULL

##' @rdname NMdataOperations
##' @export
merge.NMdata <- function(x,...){
    unNMdata(x)
    merge(x,...)
}

##' @rdname NMdataOperations
##' @export
t.NMdata <- function(x,...){
    unNMdata(x)
    t(x,...)
}

##' @rdname NMdataOperations
##' @export
dimnames.NMdata <- function(x,...){
    unNMdata(x)
    dimnames(x,...)
}

##' @rdname NMdataOperations
##' @export
rbind.NMdata <- function(x,...){
    
    unNMdata(x)
    rbind(x,...)
}

##' @rdname NMdataOperations
##' @export
cbind.NMdata <- function(x,...){
    unNMdata(x)
    cbind(x,...)
}

