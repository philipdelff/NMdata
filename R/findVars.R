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
##' @param cols.id Deprecated. Use by instead.
##' @param as.fun The default is to return a data.table if data is a data.table
##'     and return a data.frame in all other cases. Pass a function in as.fun to
##'     convert to something else. If data is not a data.table, the default can
##'     be configured using NMdataConf.
##' @details Use this to exclude columns that are constant within by. If
##'     by=ID, this could be to get only time-varying covariates.
##' @return a data set with as many rows as in data.
##' @examples
##' dt1 <- data.frame(ID=c(1,1,2,2),
##'                   OCC=c(1,2,1,2),
##'                ## ID level
##'                   eta1=c(1,1,3,3),
##'                ## occasion level
##'                   eta2=c(1,3,1,5),
##'                ## not used
##'                   eta3=0
##'                )
##' ## model level
##' findCovs(dt1)
##' ## ID level
##' findCovs(dt1,"ID")
##' ## acual ID level
##' findVars(findCovs(dt1,"ID"))
##' ## occasion level
##' findCovs(findVars(dt1,"ID"),c("ID","OCC"))
##' @family DataCreate
##' @import data.table
##' @export


findVars <- function(data,by=NULL,cols.id,as.fun=NULL){

    ## check arguments
    if(!missing(cols.id) && !is.null(by)) stop("\"cols.id\" is a deprecated name for the \"by\" argument. Just use \"by\"")
    if(!missing(cols.id)) {
        warning("\"cols.id\" argument deprecated. Use \"by\" instead.")
        by <- cols.id
    }
    
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

