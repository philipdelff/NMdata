##' Extract columns that vary within values of other columns in a data.frame
##'
##' If you want to look at the variability of a number of columns and you want
##' to disregard those that are constant. Like for findCovs, by can be of
##' arbitrary length.
##'
##' @param data data.frame in which to look for covariates
##' @param by optional covariates will be searched for in combinations of
##'     values in these columns. Often by will be either empty or ID. But
##'     it can also be both say c("ID","DRUG") or c("ID","TRT").
##' @param as.fun The default is to return a data.table if data is a data.table
##'     and return a data.frame in all other cases. Pass a function in as.fun to
##'     convert to something else. If data is not a data.table, the default can
##'     be configued using NMdataConf.
##' @details Use this to exclude columns that are constant within by. If
##'     by=ID, this could be to get only time-varying covariates.
##' @return a data set with as many rows as in data.
##' @family DataCreate
##' @import data.table
##' @export



findVars <- function(data,by=NULL,as.fun=NULL){

    ## check arguments
    if(!is.data.frame(data)){
        stop("data must be a data.frame (or data.table)")
    }

    was.data.table <- TRUE
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        was.data.table <- FALSE
        data <- as.data.table(data)
    }

    
    ## The way this is done below requires a by column to exist. So if
    ## by is NULL, we make a constant dummy.
    rm.tmp <- FALSE
    if(is.null(by)){
        by <- tmpcol(data)
        data[,(by):=1]
        rm.tmp <- TRUE
    }
    
    ## uniqueN > 1
    dt2 <- data[, .SD[, lapply(.SD, function(x)uniqueN(x)>1)], by=by]
    ## use any
    ifkeep <- dt2[,sapply(.SD,any),.SDcols=!(by)]
    keep <- c(by,setdiff(colnames(dt2),by)[ifkeep])
    reduced <- unique(data[,keep,with=FALSE])

    if(rm.tmp) reduced[,(by):=NULL]

    if(was.data.table && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    reduced <- as.fun(reduced)

    reduced
}

