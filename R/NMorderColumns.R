##' Order columns in dataset for use in Nonmem.
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
##'     representations ("asian", "caucasian", etc.) variables and the
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
##' @param nomtime The name of the column containing nominal time. If
##'     given, it will put the column quite far left, just after row
##'     counter and ID.
##' @param row A row counter column. This will be the first column in
##'     the dataset. Technically, you can use it for whatever column
##'     you want first.
##' @param as.fun The default is to return data in a data.table. Pass
##'     a function in as.fun to convert to something else. If
##'     data.frames are wanted, use as.fun=as.data.frame.
##' @details This function will change the order of columns but it
##'     will never edit values in any columns.
##'
##' The ordering is by the following steps, each step depending on
##' corresponding argument. Not
##' \itemize{
##'  \item{"row - "}{Row id if argument row is non-NULL}
##'  \item{"not editable - "}{ID (if a column is called ID)}
##'  \item{"nomtime - "}{Nominal time.}
##'  \item{"first - "}{user-specified first columns}
##'  \item{"not editable - "}{Standard Nonmem columns: TIME, EVID, CMT, AMT, RATE, DV, MDV}
##'  \item{"last - "}{user-specified last columns}
##'  \item{"chars.last - "}{numeric, or interpretable as numeric}
##'  \item{"not editable - "}{less often used nonmem names: FLAG, OCC, ROUTE, GRP, TRIAL, DRUG, STUDY}
##'  \item{"lower.last - "}{lower case in name}
##'  \item{"not editable - "}{Alphabetic/numeric sorting}
##' }
##'
##' @family DataCreate
##' @import data.table 
##' @export


NMorderColumns <- function(data,first,last,lower.last=FALSE,
                           chars.last=TRUE, nomtime="NOMTIME",
                           row="ROW", as.fun=NULL){

    was.data.frame <- FALSE
    if(is.data.table(data)){
       data <- copy(data) 
    } else {
        data <- as.data.table(data)
        was.data.frame <- TRUE
    }
    if(missing(first)) first <- NULL
    if(missing(last)) last <- NULL
    
    first1 <- c(row,"ID",nomtime,"TIME","EVID","CMT","AMT","RATE",
                "DV","MDV")
    first2 <- c("FLAG","OCC","ROUTE","GRP","TRIAL","DRUG","STUDY")

    nms <- names(data)
    missing <- setdiff(setdiff(first1,"RATE"),nms)
    if(length(missing)) warning(paste0("These standard nonmem columns were not found in data:\n",paste(missing,collapse="\n")))

    first <- c(first1,first)

    dt.names <- data.table(name=colnames(data))
    dt.names[,nfirst:=match(name,first)]
    dt.names[,nfirst2:=match(name,first2)]
    dt.names[,nlast:=match(name,last)]
    dt.names[,islower:=NA_real_]
    if(lower.last){
        dt.names[,islower:=grepl("[a-z]",name) ]
    }
    ## chars.last: If columns cannot be converted to numerics

    if(chars.last){
        dt.num.w <- data[,lapply(.SD,NMisNumeric)]
        dt.num <- data.table(name=colnames(dt.num.w),isnum=as.logical(dt.num.w[1,]))
        dt.names <- mergeCheck(dt.names,dt.num,by="name")
    } else {
        dt.names[,isnum:=TRUE]
    }

    ## order data by dt.names
    setorder(dt.names,nfirst,-nlast,-isnum,nfirst2,islower,name,na.last=TRUE)
    setcolorder(data,dt.names[,name])

    if(was.data.frame) {data <- as.data.frame(data)}
    data <- runAsFun(data,as.fun=as.fun)

    data
}
