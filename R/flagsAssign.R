##' Assign exclusion flags to a dataset based on specified table
##'
##' The aim with this function is to take a (say PK) dataset and a
##' pre-specified table of flags, assign the flags automatically.
##'
##' @param data The dataset to assign flags to.
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
##' @param col.id The name of the subject ID column. Default is
##'     "ID". This column must contain unique subject identifiers. NA
##'     and empty strings are not allowed.
##' @param col.dv The name of the data value column. Default is "DV".
##' @param return.all If TRUE, both the edited dataset and the table
##'     of flags are returned. If FALSE (default) only the edited
##'     dataset is returned.
##' @param as.fun The default is to return data.tables if input data
##'     is a data.table, and return a data.frame for all other input
##'     classes. Pass a function in as.fun to convert to something
##'     else. If return.all=F, this is applied to data and tab.flags
##'     independently.
##' @return The dataset with flags added. See parameter flags.return
##'     as well.
##' @import data.table
##' @family DataCreate
##' @examples
##' pk <- readRDS(file=system.file("examples/data/xgxr2.rds",package="NMdata"))
##' dt.flags <- data.frame(
##'        flagn=10,
##'        flagc="Below LLOQ",
##'        condition=c("BLQ==1")
##' )
##' pk <- flagsAssign(pk,dt.flags,col.flagn="flagn",col.flagc="flagc")
##' unique(pk[,c("flagn","flagc","BLQ")])
##' flagsCount(pk,dt.flags,col.flagn="flagn",col.flagc="flagc")
##' @export


flagsAssign <- function(data, tab.flags, return.all=F, col.id="ID",
                        col.dv="DV", col.flagn, col.flagc,
                        as.fun=NULL){
    
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
    as.fun.arg <- as.fun
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(col.flagn)) col.flagn <- NULL
    if(missing(col.flagc)) col.flagc <- NULL
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)
    col.flagc <- NMdataDecideOption("col.flagc",col.flagc)
####### check args end ######
    
####### Check data ######
    if(!is.data.frame(data)){stop("data must be a data.frame")}
    ## make sure data is a data.table
    
    data.was.data.table <- TRUE
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data.was.data.table <- FALSE
        data <- as.data.table(data)
    }

    
    datacols <- copy(colnames(data))
    
    if(!col.id%in%datacols) stop("data must contain a column name matching the argument col.id.")
    ## Check NA ids. I think this requires that col.id has length 1
    stopifnot(length(col.id)==1)
    if(data[,any(is.na(..col.id))]||is.character(data[,is.character(..col.id)])&&data[,any(..col.id=="")]) stop("col.id contains missing (NA's or empty strings). You must fix this first.")

    if(!col.dv%in%datacols) stop(paste(col.dv,"does not exist. Please see argument col.dv."))

### data can contain a column named FLAG - but it is removed
    if(col.flagn%in%datacols) {
        ## message("Data contains FLAG already. This is overwritten")
        messageWrap(sprintf("Data contains %s already. This is overwritten",col.flagn),fun.msg=message)
        data[,(col.flagn):=NULL]
    }
    if(col.flagc%in%datacols) {
        ## message("Data contains flag already. This is overwritten")
        messageWrap(sprintf("Data contains %s already. This is overwritten",col.flagc),fun.msg=message)
        data[,(col.flagc):=NULL]
    }
    
##### End Check data #######
    

####### Check tab.flags ####
    ## Check that tab.flags contain a numeric called FLAG and a character/factor called flag.
    if(!is.data.frame(tab.flags)||!(all(c(col.flagn,col.flagc,"condition")%in%colnames(tab.flags)))){
        ## stop("tab.flags must be a data.frame containing FLAG, flag, and condition.")
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

    
    
### For here, FLAG and flag have to be generalized to match args. These are arg checks.
    ## maybe later? rename tab.flags columns to FLAG and flag
    
##     if(!is.numeric(tab.flags[,get(col.flagn)])) stop("column FLAG in tab.flags must be numeric and non-negative")
    if(!is.numeric(tab.flags[,get(col.flagn)])) stop(sprintf("column %s in tab.flags must be numeric and non-negative",tab.flagn))
    if(any(tab.flags[,get(col.flagn)]<0)) stop(sprintf("column %s in tab.flags must be non-negative",col.flagn))
    if(!is.character(tab.flags[,get(col.flagc)])) stop(sprintf("column %s in tab.flags must be of type character",flag))
    if(!is.character(tab.flags[,condition])) stop("column expression in tab.flags must be of type character")
    
###### Check tab.flags: FLAG, flag, and condition contain unique values
    any.dups <- tab.flags[,lapply(.SD,function(x)any(duplicated(x))),.SDcols=c(col.flagn,col.flagc,"condition")][,any(c(get(col.flagn),get(col.flagc),condition))]
    if(any.dups){
        messageWrap(sprintf("Duplicate values not allowed in tab.flags columns %s, %s, and condition.",col.flagn,col.flagc),stop)
    }
####### END Check tab.flags ####
    
    
####################### CHECKS END ######################

### add an increasing variable to data so we can arrange the observations
### exactly as they were to begin with.
#### save order for re-arranging in the end
    col.row <- tmpcol(data)
    data[,(col.row):=1:.N ]
#### 

    ## we want to use columns FLAG and flag. So if these exist in
    ## data, copy for backup
    backed.up.old.flags <- FALSE
    if(any(c("FLAG","flag")%in%colnames(data))){
        backed.up.old.flags <- TRUE
        cols.pick <- c(col.row,intersect(c("FLAG","flag"),colnames(data)))
        flags.orig.data <- data[,cols.pick,with=FALSE]
        data[,FLAG:=NULL]
        data[,flag:=NULL]
    }

    
    ## rename tab.flags columns to flag and FLAG
    
    setnames(tab.flags,c(col.flagn,col.flagc),c("FLAG","flag"))
    
### FLAG==0 cannot be customized. If not in table, put in table. Return the
### table as well. Maybe a reduced table containing only used FLAGS
    if(!0%in%tab.flags[,"FLAG"]) {
        tab.flags <- rbind(
            data.table(FLAG=0,
                       flag="Analysis set",condition=NA_character_),
            tab.flags,
            fill=T)
    }
    tab.flags[FLAG==0,condition:=NA_character_]

    
    ## If a FLAG is not zero and does not have a condition, it is not used.
    tab.flags <- tab.flags[FLAG==0|(!is.na(condition)&condition!="")]
    tab.flags <- tab.flags[order(FLAG),]

    tab.flags[,condition.used := paste0("FLAG==0&(",tab.flags[,condition],")")]
    tab.flags[FLAG==0,condition.used:=NA_character_]

    
### assigning the flags
    data[,FLAG:=0]
    tab.flags.0 <- tab.flags[FLAG==0]
    tab.flags <- tab.flags[FLAG!=0]
    
    tab.flags[,Nmatched:=NA_real_]
    tab.flags[,Nobs:=NA_real_]
    tab.flags[,NID:=NA_real_]
    for(fn in 1:tab.flags[,.N]){
        
        messageWrap(
            ## paste("Coding FLAG =",tab.flags[fn,FLAG],", flag =",tab.flags[fn,flag])
            sprintf("Coding %s = %d, %s = %s",col.flagn,tab.flags[fn,FLAG],col.flagc,tab.flags[fn,flag])
           ,fun.msg=message)
        ## find all affected columns
        is.matched <- try(with(data,eval(parse(text=tab.flags[fn,condition.used]))),silent=T)
        if("try-error"%in%class(is.matched)){
            messageWrap(attr(is.matched,"condition")$message,fun.msg=warning)
            next
        }
        if(any(is.na(is.matched))) stop("Evaluation of criterion returned NA. Missing values in columns used for evaluation?")
        tab.flags[fn,Nmatched:=sum(is.matched)]
        data[is.matched,FLAG:=tab.flags[fn,FLAG]]
        tab.flags[fn,Nobs:=data[FLAG==0,.N]]
        tab.flags[fn,NID:=data[FLAG==0,uniqueN(col.id)]]
    }

    tab.flags.0[,Nmatched:=data[FLAG==0,.N]]
    
    tab.flags <- rbind(tab.flags,tab.flags.0,fill=T)

### check that all data$FLAG have a value matching tab.flags$FLAG. Then merge on the flag values.
    if(any(is.na(data[,FLAG]))) {
        ## stop("NA's found in data$FLAG after assigning FLAGS. Bug in flagsAssign?")
        messageWrap(sprintf("NA's found in %s after assigning flags. Bug in flagsAssign?",col.flagn),fun.msg=stop)
    }

    dim0 <- dim(data)
    data <- mergeCheck(data,unique(tab.flags[,c("FLAG","flag")]),all.x=T,by="FLAG")
    stopifnot(all(dim(data)==(dim0+c(0,1))))

### rename FLAG and flag, and add back backed up columns if relevant
    setnames(data,c("FLAG","flag"),c(col.flagn,col.flagc))
    setnames(tab.flags,c("FLAG","flag"),c(col.flagn,col.flagc))
    if(backed.up.old.flags){
        data <- mergeCheck(data,flags.orig.data,by=col.row)
    }
    
### arrange back to original order
    setorderv(data,col.row)
    data[,(col.row):=NULL]
    ## order columns
    
    setcolorder(data,datacols)
    
    if(!data.was.data.table || !is.null(as.fun.arg) ) {
        data <- as.fun(data)
        tab.flags <- as.fun(tab.flags)
    }

    
    if(return.all){
        if(!tab.flags.was.data.table || !is.null(as.fun.arg) ) {
            tab.flags <- as.fun(tab.flags,as.fun)
        }
        return(list(data,tab.flags))
    } else {
        return(data)
    }

}

