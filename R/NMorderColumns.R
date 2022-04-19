##' Standardize column order in Nonmem input data
##'
##' Order data columns for easy export to Nonmem. No data values are
##' edited. The order is configurable through multiple arguments. See
##' details.
##' 
##' @param data The dataset which columns to reorder.
##' @param first Columns that should come almost first. See details.
##' @param last Columns to move to back of dataset. If you work with a
##'     large dataset, and some columns are irrelevant for the Nonmem
##'     runs, you can use this argument.
##' @param lower.last Should columns which names contain lowercase
##'     characters be moved towards the back? Some people use a
##'     standard of lowercase variables (say "race") being character
##'     representations ("Asian", "Caucasian", etc.) variables and the
##'     uppercase (1,2,...) being the numeric representation for
##'     Nonmem.
##' @param chars.last Should columns which cannot be converted to
##'     numeric be put towards the end? A column can be a character or
##'     a factor in R, but still be valid in Nonmem (often the case
##'     for ID which can only contain numeric digits but really is a
##'     character or factor). So rather than only looking at the
##'     column class, the columns are attempted converted to
##'     numeric. Notice, it will attempted to be converted to numeric
##'     to test whether Nonmem will be able to make sense of it, but
##'     the values in the resulting dataset will be untouched. No
##'     values will be edited. If TRUE, logicals will always be put
##'     last. NA's must be NA or ".".
##' @param alpha Sort columns alphabetically. Notice, this is the last
##'     order priority applied.
##' @param col.nomtime The name of the column containing nominal
##'     time. If given, it will put the column quite far left, just
##'     after row counter and ID. Default value is NOMTIME and can be
##'     configured with NMdataConf.
##' @param col.row A row counter column. This will be the first column
##'     in the dataset. Technically, you can use it for whatever
##'     column you want first. Default value is ROW and can be
##'     configured with NMdataConf.
##' @param col.flagn The name of the column containing numerical flag
##'     values for data row omission. Default value is FLAG and can be
##'     configured with NMdataConf.
##' @param col.dv a vector of column names to put early to represent
##'     dependent variable(s). Default is DV.
##' @param as.fun The default is to return a data.table if data is a
##'     data.table and return a data.frame in all other cases. Pass a
##'     function in as.fun to convert to something else. The default
##'     can be configured using NMdataConf. However, if data is a
##'     data.table, settings via NMdataConf are ignored.
##' @param quiet If true, no warning will be given about missing
##'     standard Nonmem columns.
##' @details This function will change the order of columns but it
##'     will never edit values in any columns. The ordering is by the
##'     following steps, each step depending on corresponding
##'     argument.
##'
##' \itemize{
##'  \item{"col.row - "}{Row id if argument row is non-NULL}
##'  \item{"not editable - "}{ID (if a column is called ID)}
##'  \item{"col.ntime - "}{Nominal time.}
##'  \item{"first - "}{user-specified first columns}
##'  \item{"not editable - "}{Standard Nonmem columns: TIME, EVID, CMT, AMT, RATE, DV, MDV}
##'  \item{"last - "}{user-specified last columns}
##'  \item{"chars.last - "}{numeric, or interpretable as numeric}
##'  \item{"not editable - "}{less often used nonmem names: col.flagn, OCC, ROUTE, GRP, TRIAL, DRUG, STUDY}
##'  \item{"lower.last - "}{lower case in name}
##'  \item{"alpha - "}{Alphabetic/numeric sorting}
##' }
##' @return data with modified column order.
##'
##' @family DataCreate
##' @import data.table 
##' @export


NMorderColumns <- function(data,
                           first,
                           last,
                           lower.last=FALSE,
                           chars.last=TRUE,
                           alpha=TRUE,
                           col.nomtime,
                           col.row,
                           col.flagn,
                           col.dv="DV",
                           as.fun=NULL,
                           quiet){

    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    nfirst <- NULL
    name <- NULL
    nfirst2 <- NULL
    nlast <- NULL
    islower <- NULL
    isnum <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    if(missing(col.flagn)) col.flagn <- NULL
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)
    if(missing(col.nomtime)) col.nomtime <- NULL
    col.nomtime <- NMdataDecideOption("col.nomtime",col.nomtime)
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    
    if(missing(col.row)) col.row <- NULL
    col.row <- NMdataDecideOption("col.row",col.row)

    
    was.dt <- FALSE
    if(is.data.table(data)){
        data <- copy(data) 
        was.dt <- TRUE
    } else {
        data <- as.data.table(data)
    }
    if(missing(first)) first <- NULL
    if(missing(last)) last <- NULL
    
    first1 <- c(col.row,"ID",col.nomtime,"TIME","EVID","CMT","AMT","II","ADDL","RATE",
                "SS",col.dv,"MDV")
    first2 <- c(col.flagn,"OCC","GRP","TRIAL","STUDY","DRUG","ROUTE")

    
    nms <- names(data)
    nms.dup <- nms[duplicated(nms)]
    if(!quiet && length(nms.dup)) messageWrap(paste0("Duplicated column names:\n",paste(nms.dup,collapse=", ")),fun.msg=warning)
    ### these checks are now done in NMcheckData
    ## missing <- setdiff(setdiff(first1,c("II","ADDL","RATE","SS")),nms)
    ## if(!quiet && length(missing)) messageWrap(paste0("These standard nonmem columns were not found in data:\n",paste(missing,collapse="\n")),fun.msg=message)

    first <- c(first1,first)

    dt.names <- data.table(name=colnames(data))
    if(chars.last){
        ## chars.last: If columns cannot be converted to numerics
        dt.num.w <- data[,lapply(.SD,NMisNumeric)]
        dt.names[,isnum:=as.logical(dt.num.w[1,])]
    } else {
        dt.names[,isnum:=TRUE]
    }
    dt.names[,nfirst:=match(name,first)]
    dt.names[,nfirst2:=match(name,first2)]
    dt.names[,nlast:=match(name,last)]
    dt.names[is.na(nlast),nlast:=0]
    dt.names[,islower:=NA_real_]
    if(lower.last){
        dt.names[,islower:=grepl("[a-z]",name) ]
    }

    if(!alpha) dt.names[,name:=""]

    
    
    ## order data by dt.names. Making sure this works even with
    ## duplicate column names.
    
    dt.names[,order:=frank(dt.names,nfirst,-isnum,nlast,nfirst2,islower,name,na.last=TRUE,ties.method="first")]
    setcolorder(data,order(dt.names[,order]))

    if(!was.dt || !is.null(as.fun)) {
        as.fun <- NMdataDecideOption("as.fun",as.fun)
        data <- as.fun(data)
    }

    data
}
