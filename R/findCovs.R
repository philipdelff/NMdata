##' Extract columns that do not vary within variables in a data.frame
##'
##' @param data data.frame in which to look for covariates
##' @param cols.id covariates will be searched for in combinations of values in
##'     these columns. Often cols.id will be either empty or ID. But it
##'     can also be both say c("ID","DRUG") or c("ID","TRT").
##' @param debug start by running browser()?
##' @family DataWrangling
##' @import data.table
##' @export

##' @examples
##' \dontrun{
##' dat <- NMreadTab("TABLE_OUTPUT.txt")
##' ### very common use
##' findCovs(dat,cols.id="ID")
##' ###  an ID column is not needed.
##' findCovs(dat,cols.id=c())
##' ### need a new data.frame to test for length(cols.id)>1
##' }




findCovs <- function(data,cols.id=NULL,debug=F){
    if(debug) browser()
    ## check arguments
    if(!is.data.frame(data)){
        stop("data must be a data.frame (or data.table)")
    }
    
    was.data.table <- T
    if(!is.data.table(data)){
        was.data.table <- F
        data <- as.data.table(data)
    }

    cnames <- colnames(data)
    cnames.to.use <- setdiff(cnames,cols.id)
    
    if(is.null(cols.id)){
        Nid <- 1
    } else {
        ## This is a little clumpsy, but it works when cols.id is of length > 1.
        Nid <- nrow(unique(data[,cols.id,with=F]))
    }

    names.covs <- cnames.to.use[unlist(lapply(cnames.to.use,function(x) nrow(unique(data[,c(cols.id,x),with=F]))==Nid))]

    reduced <- unique(data[,c(cols.id,names.covs),with=F])
    if(!is.null(cols.id)){
        reduced <- reduced[order(get(cols.id))]
    }

    if(!was.data.table) reduced <- as.data.frame(reduced)
    reduced

}

