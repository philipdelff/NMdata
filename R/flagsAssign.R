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
##' @param return.all If TRUE, both the edited dataset and the table
##'     of flags are returned. If FALSE (default) only the edited
##'     dataset is returned.
##' @param col.id The name of the subject ID column. Default is "ID".
##' @param col.dv The name of the data value column. Default is "DV".
##' @param as.fun The default is to return data in data.tables. Pass a
##'     function in as.fun to convert to something else. If
##'     data.frames are wanted, use as.fun=as.data.frame.
##' @return The dataset with flags added. See parameter flags.return
##'     as well.
##' @import data.table
##' @family DataCreate
##' @export


flagsAssign <- function(data, tab.flags, return.all=F, col.id="ID",
                        col.dv="DV", as.fun=NULL){

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

####### Check data ######
    if(!is.data.frame(data)){stop("data must be a data.frame")}
    ## make sure data is a data.table

    was.data.table <- T
    if(!is.data.table(data)){
        was.data.table <- F
        data <- as.data.table(data)
    }

    datacols <- colnames(data)
    if(!col.id%in%datacols) stop("data must contain a column name matching the argument col.id.")
    ## Check NA ids. I think this requires that col.id has length 1
    stopifnot(length(col.id)==1)
    if(data[,any(is.na(..col.id))]||is.character(data[,is.character(..col.id)])&&data[,any(..col.id=="")]) stop("col.id contains missing (NA's or empty strings). You must fix this first.")

    if(!col.dv%in%datacols) stop(paste(col.dv,"does not exist. Please see argument col.dv."))

### data can contain a column named FLAG - but it is removed
    if("FLAG"%in%datacols) {
        message("Data contains FLAG already. This is overwritten")
        data[,FLAG:=NULL]
    }
    if("flag"%in%datacols) {
        message("Data contains flag already. This is overwritten")
        data[,flag:=NULL]
    }
    
##### End Check data #######


####### Check tab.flags ####
    ## Check that tab.flags contain a numeric called FLAG and a character/factor called flag.
    if(!is.data.frame(tab.flags)||!(all(c("FLAG","flag","condition")%in%colnames(tab.flags)))){
        stop("tab.flags must be a data.frame containing FLAG, flag, and condition.")
    }
    ## make sure tab.flags and data are data.tables
    tab.flags <- as.data.table(tab.flags)

    if(!is.numeric(tab.flags[,FLAG])) stop("column FLAG in tab.flags must be numeric and non-negative")
    if(tab.flags[,any(FLAG<0)]) stop("column FLAG in tab.flags must be non-negative")
    if(!is.character(tab.flags[,flag])) stop("column flag in tab.flags must be of type character")
    if(!is.character(tab.flags[,condition])) stop("column expression in tab.flags must be of type character")
    
###### Check that FLAG, flag, and condition contain unique values
    any.dups <- tab.flags[,lapply(.SD,function(x)any(duplicated(x))),.SDcols=c("FLAG","flag","condition")][,any(c(FLAG,flag,condition))]
    if(any.dups){
        messageWrap("Duplicate values not allowed in tab.flags columns FLAG, flag, and condition.",stop)
    }
####### END Check tab.flags ####
    
    
####################### CHECKS END ######################

### add an increasing variable to data so we can arrange the observations
### exactly as they were to begin with.
#### save order for re-arranging in the end
    col.row <- tmpcol(data)
    data[,(col.row):=1:.N ]
#### 


    
    
### FLAG==0 cannot be customized. If not in table, put in table. Return the
### table as well. Maybe a reduced table containing only used FLAGS
    if(!0%in%tab.flags[,"FLAG"]) {tab.flags <- rbind(
                                      data.table(FLAG=0,flag="Analysis set",condition=NA_character_),
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
            paste("Coding FLAG =",tab.flags[fn,FLAG],", flag =",tab.flags[fn,flag])
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
        ## print(subset(data,is.na(FLAG)))
        stop("NA's found in data$FLAG after assigning FLAGS. Bug in flagsAssign?")
    }

    dim0 <- dim(data)
    data <- mergeCheck(data,unique(tab.flags[,c("FLAG","flag")]),all.x=T,by="FLAG")
    stopifnot(all(dim(data)==(dim0+c(0,1))))
### arrange back to original order
    setorderv(data,col.row)
    data[,(col.row):=NULL]

    
    if(!was.data.table) {
        data <- as.data.frame(data)
        tab.flags <- as.data.frame(tab.flags)
    }
    data <- runAsFun(data,as.fun)
    tab.flags <- runAsFun(tab.flags,as.fun)
    
    if(return.all){
        return(list(data,tab.flags))
    } else {
        return(data)
    }

}

