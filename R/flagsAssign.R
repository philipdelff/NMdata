##' Assign exclusion flags to a dataset based on specified table
##'
##' The aim with this function is to take a (say PK) dataset and a
##' pre-specified table of flags, assign the flags automatically.
##'
##' @param data The dataset to assign flags to.
##' @param subset.data An optional string that provides a subset of
##'     data to assign flags to. A common example is
##'     subset=\"EVID==0\" to only assign to observations. Numerical
##'     and character flags will be missing in rows that are not
##'     matched by this subset.
##' @param tab.flags A data.frame containing at least these named
##'     columns: FLAG, flag, condition. Condition is disregarded for
##'     FLAG==0. FLAG must be numeric and non-negative, flag and
##'     condition are characters.
##' @param col.flagn The name of the column containing the numerical
##'     flag values in tab.flags. This will be added to data. Default
##'     value is FLAG and can be configured using NMdataConf.
##' @param col.flagc The name of the column containing the character
##'     flag values in tab.flags. This will be added to data. Default
##'     value is flag and can be configured using NMdataConf.
##' @param flags.increasing The flags are applied by either decreasing
##'     (default) or increasing value of col.flagn. Decreasing order
##'     means that conditions associated with higher values of
##'     col.flagn will be evaluated first. By using decreasing order,
##'     you can easily adjust the Nonmem IGNORE statement from
##'     IGNORE(FLAG.NE.0) to say IGNORE(FLAG.GT.10) if BLQ's have
##'     FLAG=10, and you decide to include these in the analysis.
##' @param grp.incomp Column(s) that distinct incompatible subsets of
##'     data. Default is "EVID" meaning that if different values of
##'     EVID are found in data, the function will return an
##'     error. This is a safeguard not to mix data unintentionally
##'     when counting flags.
##' @param flagc.0 The character flag to assign to rows that are not
##'     matched by exclusion conditions (numerical flag 0).
##' @param as.fun The default is to return data.tables if input data
##'     is a data.table, and return a data.frame for all other input
##'     classes. Pass a function in as.fun to convert to something
##'     else. If return.all=FALSE, this is applied to data and
##'     tab.flags independently.
##' @return The dataset with flags added. Class as defined by
##'     as.fun. See parameter flags.return as well.
##' @details dt.flags must contain a column with numerical exclusion
##'     flags, one with character exclusion flags, and one with a
##'     expressions to evaluate for whether to apply the exclusion
##'     flag. The flags are applied sequentially, by increasing value
##'     of the numerical exclusion flag.
##' @import data.table
##' @family DataCreate
##' @examples
##' \dontrun{
##' pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
##' dt.flags <- data.frame(
##'        flagn=10,
##'        flagc="Below LLOQ",
##'        condition=c("BLQ==1")
##' )
##' pk <- flagsAssign(pk,dt.flags,subset.data="EVID==0",col.flagn="flagn",col.flagc="flagc")
##' pk <- flagsAssign(pk,subset.data="EVID==1",flagc.0="Dosing",
##'         col.flagn="flagn",col.flagc="flagc")
##' unique(pk[,c("EVID","flagn","flagc","BLQ")])
##' flagsCount(pk[EVID==0],dt.flags,col.flagn="flagn",col.flagc="flagc")
##' }
##' @export


flagsAssign <- function(data, tab.flags, subset.data, col.flagn, col.flagc,
                        flags.increasing=FALSE, grp.incomp="EVID",
                        flagc.0="Analysis set", as.fun=NULL){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    ..col.id <- NULL
    FLAG <- NULL
    flag <- NULL
    condition <- NULL
    condition.used <- NULL
    Nmatched <- NULL
    Nobs <- NULL
    NID <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    
##################### CHECKS START ######################

####### check args ######
    if(missing(as.fun)) as.fun <- NULL
    ## as.fun.arg <- as.fun
    if(is.null(as.fun) && is.data.table(data)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(col.flagn)) col.flagn <- NULL
    if(missing(col.flagc)) col.flagc <- NULL
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)
    col.flagc <- NMdataDecideOption("col.flagc",col.flagc)

####### check args end ######

    
####### Check data ######
    if(!is.data.frame(data)){stop("data must be a data.frame")}
    ## make sure data is a data.table

    if(nrow(data)==0) {
        warning("data is empty. Nothing to do.")
        ## as.fun <- NMdataDecideOption("as.fun",as.fun)
        data <- as.fun(data)
        return(data)
    }
    
    data.was.data.table <- TRUE
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data.was.data.table <- FALSE
        data <- as.data.table(data)
    }

    datacols <- copy(colnames(data))

##### End Check data #######
    

####### Check tab.flags ####
    if(missing(tab.flags)) {
        tab.flags <- data.table(FLAG=0, flag=flagc.0, condition="TRUE")
        setnames(tab.flags,c("FLAG","flag"),c(col.flagn,col.flagc))
    }
    ## Check that tab.flags contain a numeric called FLAG and a character/factor called flag.
    if(!is.data.frame(tab.flags)||!(all(c(col.flagn,col.flagc,"condition")%in%colnames(tab.flags)))){
        messageWrap(sprintf("tab.flags must be a data.frame containing %s, %s, and condition.",col.flagn,col.flagc),fun.msg=stop)
    }

    ## make sure tab.flags and data are data.tables
    tab.flags.was.data.table <- TRUE
    if(is.data.table(tab.flags)){
        tab.flags <- copy(tab.flags)
    } else {
        tab.flags.was.data.table <- FALSE
        tab.flags <- as.data.table(tab.flags)
    }
    
### Check that FLAG values are unique. If not, we can't merge flag on to data afterwards.
    if(tab.flags[,uniqueN(get(col.flagn))!=.N]){
        messageWrap(sprintf("All rows in tab.flags must contain unique values of %s",col.flagc),fun.msg=stop)
    }
    
### Checks of tab.flags column classes
    if(!is.numeric(tab.flags[,get(col.flagn)])) stop(sprintf("column %s in tab.flags must be numeric and non-negative",col.flagn))
    if(any(tab.flags[,get(col.flagn)]<0)) stop(sprintf("column %s in tab.flags must be non-negative",col.flagn))
    if(!is.character(tab.flags[,get(col.flagc)])) stop(sprintf("column %s in tab.flags must be of type character",col.flagc))
    if(!is.character(tab.flags[,condition])) stop("column expression in tab.flags must be of type character")
    
###### Check tab.flags: FLAG, flag, and condition contain unique values
    any.dups <- tab.flags[,lapply(.SD,function(x)any(duplicated(x))),.SDcols=c(col.flagn,col.flagc,"condition")][,any(c(get(col.flagn),get(col.flagc),condition))]
    if(any.dups){
        messageWrap(sprintf("Duplicate values not allowed in tab.flags columns %s, %s, and condition.",col.flagn,col.flagc),stop)
    }

    ####### END Check tab.flags ####
    
##### check subset
### TODO check for " *" which should work like " " but will give
### an error for now.
    if(missing(subset.data) ||
       is.null(subset.data) ||
       is.character(subset.data) && length(subset.data)==1 && subset.data==""
       ){
        subset.data <- ""
        subsetAND <- ""
    } else {
        if(!(is.character(subset.data)&&length(subset.data)==1)){
            stop("If not missing or NULL, subset.data must be one character string (e.g. \"EVID==0\").")
        }
        subsetAND <- paste(subset.data,"&")
    }
    
    if(subset.data=="") {
        data.sub <- data
    } else {
        data.sub <- data[eval(parse(text=subset.data))]
    }
    if(nrow(data.sub)==0){
        message("Data set empty (after applying subset if used).")
        return(as.fun(data))
    }

    
### check for incompatible groups (say doses and observations)
    check.incomp <- intersect(datacols,grp.incomp)
    if(length(check.incomp)>0) {
        covs.incomp <- findCovs(data.sub[,check.incomp,with=FALSE])
        if(length(check.incomp)>ncol(covs.incomp)){
            cols.incomp <- setdiff(check.incomp,colnames(covs.incomp))
            stop(sprintf("Incompatible data included. Column(s) %s is non-unique. Consider running on a subset of data or see argument grp.incomp.",paste(cols.incomp,sep=", ")))
        }
    }

    
### data can contain a column named FLAG - but it is removed
    if(col.flagn%in%datacols&&data.sub[,any(!is.na(get(col.flagn)))]) {
        messageWrap(sprintf("Data contains %s already. This is overwritten",col.flagn),fun.msg=message)
    }
    if(col.flagc%in%datacols&&data.sub[,any(!is.na(get(col.flagc)))]) {
        messageWrap(sprintf("Data contains %s already. This is overwritten",col.flagc),fun.msg=message)
    }

    
####################### CHECKS END ######################

### add an increasing variable to data so we can arrange the observations
### exactly as they were to begin with.
#### save order for re-arranging in the end
    col.row <- tmpcol(data)
    data[,(col.row):=.I ]

#### We will not touch the data not matched by the subset
    ## data.noflags <- data[eval(parse(text=paste("!",subsetAND,"1")))]
    ## data.flags <- data[eval(parse(text=paste(subsetAND,"1")))]
    if(subset.data==""){
        data.flags <- copy(data)
        data.noflags <- NULL
    } else {
        data.flags <- data[eval(parse(text=subset.data))]
        data.noflags <- data[eval(parse(text=paste0("!(",subset.data,")")))]
    }
    

    ## we want to use columns FLAG and flag. So if these exist in
    ## data, copy for backup
    backed.up.old.flags <- FALSE
    colnames.flags <- c(col.flagn,col.flagc)
    colnames.flags.work <- c("FLAG","flag")
    ## we only want to keep FLAG and flag if col.flagc and flagn are
    ## other columns. If col.flagn and col.flagc will be called FLAG
    ## and flag, they should overwrite the ecxisting anyway.
    colnames.flags.pick <- setdiff(colnames.flags.work,colnames.flags)

    
    if(length(colnames.flags.pick)>0){
        cols.pick <- c(col.row,intersect(colnames.flags.pick,colnames(data.flags)))
        ## we only want to keep FLAG and flag if col.flagc and flagn are other columns. If col.flagn and col.flagc will be called FLAG and flag, they should overwrite the ecxisting anyway.
        ## cols.pick <- setdiff(cols.pick,colnames.flags)
        if(length(cols.pick)>0){
            backed.up.old.flags <- TRUE
            flags.orig.data <- data.flags[,cols.pick,with=FALSE]
        }
        if(length(setdiff(cols.pick,col.row))){
            data.flags[,(setdiff(cols.pick,col.row)):=NULL]
        }
    }
    
    
    if(col.flagn%in%colnames(data.flags)){
        data.flags[,(col.flagn):=NULL]
    }
    if(col.flagc%in%colnames(data.flags)){
        data.flags[,(col.flagc):=NULL]
    }
    
    
    ## rename tab.flags columns to flag and FLAG
    setnames(tab.flags,c(col.flagn,col.flagc),c("FLAG","flag"))
    

### FLAG==0 cannot be customized. If not in table, put in table. Return the
### table as well. Maybe a reduced table containing only used FLAGS
    
    if(!0%in%tab.flags[,FLAG]) {
        tab.flags <- rbind(
            data.table(FLAG=0,
                       flag=flagc.0,condition=NA_character_),
            tab.flags,
            fill=TRUE)
    }

    ## tab.flags[FLAG==0,condition:=NA_character_]
    
    ## If a FLAG is not zero and does not have a condition, it is not used.
    tab.flags <- tab.flags[FLAG==0|(!is.na(condition)&condition!="")]
    if(flags.increasing){
        setorder(tab.flags,FLAG)
    } else {
        setorder(tab.flags,-FLAG)
    }

    tab.flags[,condition.used := paste0(subsetAND,"FLAG==0&(",tab.flags[,condition],")")]
    tab.flags[FLAG==0,condition.used:=NA_character_]
    tab.flags.0 <- tab.flags[FLAG==0]
    tab.flags <- tab.flags[FLAG!=0]
    
### assigning the flags
    
    data.flags[,FLAG:=0]
    
    nconds <- tab.flags[,.N]
    if(nconds>0){
        for(fn in 1:nconds){
            
            ##messageWrap(
            ## this gets so clunky in messageWrap. Running message directly.
            message(sprintf("Coding %s = %d, %s = %s",col.flagn,tab.flags[fn,FLAG],col.flagc,tab.flags[fn,flag]))
            ##,fun.msg=message)
            ## find all affected columns
            is.matched <- try(with(data.flags,eval(parse(text=tab.flags[fn,condition.used]))),silent=TRUE)
            if("try-error"%in%class(is.matched)){
                messageWrap(attr(is.matched,"condition")$message,fun.msg=warning)
                next
            }
            if(any(is.na(is.matched))) stop("Evaluation of criterion returned NA. Missing values in columns used for evaluation?")
            data.flags[is.matched,FLAG:=tab.flags[fn,FLAG]]
        }
    }

    tab.flags <- rbind(tab.flags,tab.flags.0,fill=TRUE)
    
    
### check that all data.flags$FLAG have a value matching tab.flags$FLAG. Then merge on the flag values.
    if(any(is.na(data.flags[,FLAG]))) {
        ## stop("NA's found in data.flags$FLAG after assigning FLAGS. Bug in flagsAssign?")
        messageWrap(sprintf("NA's found in %s after assigning flags. Bug in flagsAssign?",col.flagn),fun.msg=stop)
    }
    
    
    dim0 <- dim(data.flags)
    data.flags <- mergeCheck(data.flags,unique(tab.flags[,c("FLAG","flag")]),all.x=TRUE,by="FLAG",ncols.expect=1,common.cols=base::stop,quiet=TRUE)
    ##    stopifnot(all(dim(data.flags)==(dim0+c(0,1))))

### rename FLAG and flag, and add back backed up columns if relevant
    setnames(data.flags,c("FLAG","flag"),c(col.flagn,col.flagc))
    ## setnames(tab.flags,c("FLAG","flag"),c(col.flagn,col.flagc))
    if(backed.up.old.flags){
        data.flags <- mergeCheck(data.flags,flags.orig.data,by=col.row,common.cols=base::stop,quiet=TRUE)
    }

    ## add the data where flags have not been assigned
    data <- rbind(data.noflags,data.flags,fill=TRUE)
    
    ## arrange back to original order
    setorderv(data,col.row)
    data[,(col.row):=NULL]

    ## order columns
    setcolorder(data,datacols)

    data <- as.fun(data)
    return(data)

}

