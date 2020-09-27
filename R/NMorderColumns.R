##' Order columns in dataset for use in Nonmem.
##'
##' Order data columns for easy export to Nonmem. The order is configurable through multiple arguments. See details.
##' 
##' @param data The dataset which columns to reorder.
##' @param first Columns that should come before alphabetic
##'     sorting. Default is
##'     c("ROW","ID","NTIM","TIME","EVID","CMT","AMT","RATE","DV","MDV","FLAG","OCC","ROUTE","GRP","TRIAL")
##' @param last Columns to sort after alphabetic ordering. Default is
##'     none.
##' @param lower.last Should columns which names contain lowercase
##'     characters be moved toward the end? Some people use a standard
##'     of lowercase variables (say "race") being character
##'     representations ("asian", "caucasian", etc.) variables and the
##'     uppercase (1,2,...) being the numeric representation for
##'     Nonmem.
##' @param chars.last Should columns which cannot be converted to
##'     numeric be put towards the end? A column can be a character or
##'     a factor in R, but still be valied in Nonmem. So rather than
##'     only looking at the column class, the columns are attempted
##'     converted to numeric. Notice, it will attempted to be
##'     converted to numeric to test wheather Nonmem will be able to
##'     make sense of it, but the values in the resulting dataset will
##'     be untouched. No values will be edited. If TRUE, logicals will
##'     always be put last.
##' @param nomtime The name of the column containing nominal time. If
##'     given, it will put the column quite far left.
##' @param row A row counter column. This will be the first column in
##'     the dataset.
##' @param as.fun The default is to return data in data.tables. Pass a
##'     function in as.fun to convert to something else. If
##'     data.frames are wanted, use as.fun=as.data.frame.
##' @param by.ref If data is a data.table, the ordering can be
##'     performed by reference. Please understand, this will write
##'     directly to the data in your workspace. Only use this if this
##'     is understood. Whether or not, the function will be very fast.
##' @details This function will change the order of columns but it
##'     will never edit values in any columns.
##'
##' The ordering is by the following steps, each step depending on
##' corresponding argument.
##' \itemize{
##'  \item{"row - "}{Row id}
##'  \item{"not editable - "}{ID (if a column is called ID)}
##'  \item{"first - "}{user-specified first columns}
##'  \item{"last - "}{user-specified last columns}
##   \item{"chars.last - "}{numeric, or interpretable as numeric}
##'  \item{"not editable - "}{less often used nonmem names: FLAG, OCC, ROUTE,...}
##'  \item{"lower.last - "}{lower case in name}
##' }
##'
##' @family DataWrangling
##' @import data.table 
##' @export


NMorderColumns <- function(data,first,last,lower.last=FALSE,
                           chars.last=TRUE, nomtime="NOMTIME",
                           row="ROW", by.ref=FALSE, as.fun=NULL){

    was.data.frame <- FALSE
    if(is.data.table(data)&&!by.ref){
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
    if(by.ref) return(invisible(data))
    data
}
