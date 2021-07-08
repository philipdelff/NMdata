##' Extract columns that do not vary within variables in data
##'
##' This function provides an automated method to extract covariate-like
##' columns. The user decides which columns these variables cannot vary
##' within. So if you have repeated measures for each ID, this function can find
##' the columns that are constant within ID and their unique values for each
##' ID. Or, you can provide a combination of id.cols, say ID and STUDY, and get
##' variables that do not vary within unique combinations of these.
##'
##' @param data data.frame in which to look for covariates
##' @param by covariates will be searched for in combinations of values in
##'     these columns. Often by will be either empty or ID. But it can also
##'     be both say c("ID","DRUG") or c("ID","TRT").
##' @param as.fun The default is to return a data.table if data is a data.table
##'     and return a data.frame in all other cases. Pass a function in as.fun to
##'     convert to something else. If data is not a data.table, the default can
##'     be configued using NMdataConf.
##' @return a data set with one observation per combination of values of
##'     variables listed in by.
##' @family DataCreate
##' @import data.table
##' @export
##' @examples
##' dat <- NMscanData(system.file("examples/nonmem/xgxr001.lst", package = "NMdata"))
##' ### very common use
##' findCovs(dat,by="ID")
##' ### Without an ID column we get non-varying columns
##' findCovs(dat)



findCovs <- function(data,by=NULL,as.fun=NULL){

    ## check arguments
    if(!is.data.frame(data)){
        stop("data must be a data.frame (or data.table)")
    }

    was.data.table <- TRUE
    if(!is.data.table(data)){
        was.data.table <- FALSE
        data <- as.data.table(data)
    }

    cnames <- colnames(data)
    cnames.to.use <- setdiff(cnames,by)
    
    if(is.null(by)){
        Nid <- 1
    } else {
        ## This is a little clumpsy, but it works when by is of length > 1.
        Nid <- nrow(unique(data[,by,with=FALSE]))
    }

    names.covs <- cnames.to.use[unlist(lapply(cnames.to.use,function(x) nrow(unique(data[,c(by,x),with=FALSE]))==Nid))]

    reduced <- unique(data[,c(by,names.covs),with=FALSE])
    if(!is.null(by)){
        reduced <- reduced[order(get(by))]
    }

    
    if(was.data.table && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    reduced <- as.fun(reduced)

    reduced

}

