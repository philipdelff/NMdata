##' Extract columns that vary within values of other columns in a data.frame
##'
##' If you want to look at the variability of a number of columns and you want
##' to disregard those that are constant. Like for findCovs, cols.id can be of
##' arbitrary length.
##'
##' @param data data.frame in which to look for covariates
##' @param cols.id optional covariates will be searched for in combinations of
##'     values in these columns. Often cols.id will be either empty or ID. But
##'     it can also be both say c("ID","DRUG") or c("ID","TRT").
##' @param as.fun The default is to return a data.table if data is a data.table
##'     and return a data.frame in all other cases. Pass a function in as.fun to
##'     convert to something else. If data is not a data.table, the default can
##'     be configued using NMdataConf.
##' @details Use this to exclude columns that are constant within cols.id. If
##'     cols.id=ID, this could be to get only time-varying covariates.
##' @return a data set with as many rows as in data.
##' @family DataCreate
##' @import data.table
##' @export



findVars <- function(data,cols.id=NULL,as.fun=NULL){

    ## check arguments
    if(!is.data.frame(data)){
        stop("data must be a data.frame (or data.table)")
    }

    was.data.table <- T
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        was.data.table <- F
        data <- as.data.table(data)
    }

    
    ## The way this is done below requires a by column to exist. So if
    ## by is NULL, we make a constant dummy.
    rm.tmp <- FALSE
    if(is.null(cols.id)){
        cols.id <- tmpcol(data)
        data[,(cols.id):=1]
        rm.tmp <- TRUE
    }
    
    ## uniqueN > 1
    dt2 <- data[, .SD[, lapply(.SD, function(x)uniqueN(x)>1)], by=cols.id]
    ## use any
    ifkeep <- dt2[,sapply(.SD,any),.SDcols=!(cols.id)]
    keep <- c(cols.id,setdiff(colnames(dt2),cols.id)[ifkeep])
    reduced <- unique(data[,keep,with=F])

    if(rm.tmp) reduced[,(cols.id):=NULL]

    if(was.data.table && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    reduced <- as.fun(reduced)

    reduced
}

