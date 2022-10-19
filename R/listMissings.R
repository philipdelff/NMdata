##' List rows with missing values across multiple columns
##'
##' Missing can be NA and for character variables it can be certain
##' strings too. This function is experimental and design may change
##' in future releases.
##' 
##' @param data The data to look into.
##' @param cols The columns to look for missings in.
##' @param by If supplied, we are keeping track of the missings within
##'     the values of the by columns. In summary, by is included too.
##' @param na.strings Strings that should be interpreted as
##'     missing. All spaces will be removed before we compare to
##'     na.strings. The default is c("",".") so say " .  " is a
##'     missing by default.
##' @param quiet Keep quiet? Default is not to.
##' @param as.fun A function that will be run on the result before
##'     returning. If first input data set is a data.table, the
##'     default is to return a data.table, if not the default is to
##'     return a data.frame. Use whatever to get what fits in with
##'     your workflow. Default can be configured with NMdataConf.
##' @family DataWrangling
##' @return Invisibly, a data.frame including all findings
##' @import data.table
##' @export


listMissings <- function(data,cols,by,na.strings=c("","."),quiet=FALSE,as.fun){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    dots <- NULL
    value <- NULL
    . <- NULL

###  Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
#### Section start: Checking arguments ####

    data <- copy(as.data.table(data))
    if(missing(cols)) cols <- copy(colnames(data))

    if(is.data.table(dots[[1]]) && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(missing(by)){
        by <- "by"
        data[,(by):=1]
        had.by <- FALSE
    } else {
        had.by <- TRUE
        if(!all(by%in%colnames(data))){
            stop("all elements in by must be existing column names in data.")
        }
    }

    if(any(duplicated(cols))){
        warning("Removing replications of values in cols.")
        cols <- unique(cols)
    }

    ## not sure this will work if there are duplicate column names in data
    cols.matched <- intersect(cols,colnames(data))
    if(any(duplicated(cols.matched))){
        stop("One or more of the column names to be checked is or refer to multiple columns in data.",paste(unique(cols.matched[duplicated(cols.matched)]),collapse=", "))
    }
    
###  Section end: Checking arguments
    
    is.missing <- function(x) {
        is.na(x) |
            (is.character(x) & gsub(" ","",x)%in%c(na.strings))
    }


    data <- data[,(cols):=lapply(.SD,as.character),.SDcols=cols]
    data[,row:=.I]
    data.l <- melt(data[,c(by,"row",cols),with=F],id.vars=c(by,"row"))
    res <- data.l[is.missing(value),c(by,"variable","row","value"),with=F]

    if(nrow(res)==0){
        if(!quiet) message("No missing values identified")
        return(invisible(NULL))
    }
    
    setorderv(res,c(by,"variable","row"))
    
    bys <- c(by,"variable")
    if(!had.by) {
        res[,(by):=NULL]
        bys <- setdiff(bys,by)
    }
    
    if(!quiet){
        print(
            res[,.(Nmissing=.N),by=bys]
        )
    }
    
    as.fun(res)

}
