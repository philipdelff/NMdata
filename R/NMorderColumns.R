##' Order columns in dataset for use in Nonmem.
##'
##' Order the columns in a data.frame for easy export to Nonmem. Standard
##' columns like "ROW", "ID", "NTIM", etc. will be first by default, then other
##' capital case named columns, then lowercase named columns (one or more
##' lowercase letter means that column is sorted as lowercase). Except for
##' columns mentioned in "first" and "last" arguments, columns are sorted
##' alphabetically (after by case). In short, priority is 1: Case, first/last,
##' alphabetical. This means that last="BW" will put body weight as last of
##' capital, but before lowercase columns.
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
##' @param debug Start by calling browser()?
##' @details This function will change the order of columns but it
##'     will never edit values in any columns.
##' @family DataWrangling
##' @importFrom data.table is.data.table
##' @export


NMorderColumns <- function(data,first,last,lower.last=T,chars.last=T,
                           nomtime="NOMTIME",row="ROW",debug=F){
    if(debug) browser()
    first1 <- c(row,"ID",nomtime,"TIME","EVID","CMT","AMT","RATE",
                "DV","MDV")
    if(!missing(first)){
        first1 <- c(first1,first)
        ## first2
        nms <- names(data)
        missing <- setdiff(setdiff(first1,"RATE"),nms)
        if(length(missing)) warning(paste0("These standard nonmem columns were not found in data:\n",paste(missing,collapse="\n")))
        first2 <- c("FLAG","OCC","ROUTE","GRP","TRIAL","DRUG","STUDY")
        first <- c(first1,first2)
    } else {
        first <- first1
    }
    ## others in alphebetically sorted 
    ## last is if some should be after the alphebetically ordered
    if(missing(last)){
        last <- c()
    }
    ## Then follows whatever variables contain a lowercase letter
    nms <- names(data)
### checks of existense of standard columns

    firstpts <- match(nms,c(first))
    lastpts <- match(nms,c(last))
    lowerpts <- rep(NA,length(lastpts))
    if(lower.last){
        lowerpts[grep("[a-z]",names(data))] <- 1
        lowerpts[grep("[\\.]",names(data))] <- 1
    }
    notmatched <- rowSums(cbind(firstpts,lastpts),na.rm=TRUE)==0
    middlepts <- numeric(length(firstpts))
    middlepts[which(notmatched)[order(nms[notmatched])]] <- 1:sum(notmatched)

    ord <- order(rowSums(cbind(firstpts,middlepts*1E3,lastpts*1E5,lowerpts*1e7),na.rm=TRUE))
    if(is.data.table(data)){
        ##  data.out <- data[,nms[ord],with=F]
        setcolorder(data,nms[ord])
        data.out <- data
        was.data.frame <- FALSE
    } else {
        data.out <- data[,nms[ord]]
        was.data.frame <- TRUE
    }

### there is no is.POSIXct function available in base R. There is one in lubridate, but not to depend on that, we do a simple one here
    is.timestamp <- function(x){
        inherits(x, "POSIXct") ||
            inherits(x, "POSIXlt") ||
            inherits(x, "POSIXt")  ||
            inherits(x, "Date")
        }
    
##### chars.last: If columns cannot be converted to numerics, they are very last.
    if(chars.last){
        if(was.data.frame) data.out <- as.data.table(data.out)
        
        
        cnames <- colnames(data.out)
        types <- sapply(data.out,class)
        ## types
        ## logicals have to go last, so no testing
        cols.char <- cnames[!types%in%c("numeric","integer","logical")]
        ## cols.char
        suppressWarnings({
            if.penal <- data.out[,lapply(.SD,function(x)any(is.na(as.numeric(as.character(x[!is.na(x)]))))||is.timestamp(x)),.SDcols=cols.char]
        })
        to.penal <- names(if.penal)[unlist(if.penal)]
        to.penal <- c(to.penal,cnames[types=="logical"])
        if(length(to.penal)){        
            no.penal <- setdiff(cnames,to.penal)
            message("The following columns will be last because they could not be converted to numeric:\n",paste(to.penal,collapse=", "))
            setcolorder(data.out,c(no.penal,to.penal))
        }
        if(was.data.frame) {data.out <- as.data.frame(data.out)}
        
    }
    
    data.out
}
