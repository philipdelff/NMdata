##' Add cumulative number of doses, time of last dose and time since last dose to data
##'
##' For now, doses have to be in data as EVID=1 and/or EVID=4
##' records. They can be in the format of one row per dose or repeated
##' dosing notation using ADDL and II.
##' @param data The data set to add the variables to.
##' @param col.time Name of time column (created by addTAD). Default
##'     it TIME.
##' @param col.tdos Name of the time of previous dose column (created
##'     by addTAD). Default is TDOS.
##' @param col.tad Name of the time of prvious dose column (created by
##'     addTAD). Default is TAD.
##' @param col.ndoses The name of the column (created by addTAD) that
##'     holds the cumulative number of doses administered to the
##'     subject.
##' @param col.evid The name of the event ID column. This must exist
##'     in data. Default is EVID.
##' @param col.amt col.evid The name of the dose amount column. This
##'     must exist in data. Default is AMT.
##' @param subset.dos A string that will be evaluated as a custom
##'     expression to identify relevant events. See subset.is.complete
##'     as well.
##' @param subset.is.complete Only used in combination with
##'     non-missing subset.dos. By default, subset.dos is used in
##'     addition to the impact of col.evid and col.amt. If
##'     subset.is.complete=TRUE, subset.dos is used alone, and
##'     col.evid and col.amt are completely ignored. This is typically
##'     useful if the events are not doses but other events that are
##'     not expressed as a typical dose combination of EVID and AMT
##'     columns.
##' @param by Columns to do calculations within. Default is ID.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @import data.table
##' @details This is still experimental.
##' @export


addTAD <- function(data,col.time="TIME",col.tdos="TDOS",col.tad="TAD",col.ndoses="NDOSES",col.evid="EVID",col.amt="AMT",subset.dos,subset.is.complete,by="ID",as.fun){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    NDOSES <- NULL
    TDOS <- NULL
    TAD <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(!missing(subset.is.complete)&&missing(subset.dos)) {
        messageWrap("subset.is.complete can only be used in combination with subset.dos.",fun.msg=stop)
    }
    if(missing(subset.is.complete)) subset.is.complete <- FALSE
    
    subset.event.0 <- sprintf("%s%%in%%c(1,4)&%s>0",col.evid,col.amt)
    if(subset.is.complete) {
        subset.event <- subset.dos
    } else if(!missing(subset.dos)) {
        subset.event <- paste0(subset.dos,subset.event.0,sep="&")
    } else {
        subset.event <- subset.event.0
    }
    
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)   
    }    

    ## row identifier for reordering data back to original order after modifications
    col.row.tmp <- tmpcol(data)
    data[,(col.row.tmp):=.I]

    col.event <- tmpcol(data,base="event")
    data[,(col.event):=FALSE]
    data[eval(parse(text=subset.event)),(col.event):=TRUE]
    
### quit if no doses found etc
    
    
    ## expand doses if necessary
    data2 <- NMexpandDoses(data=data,quiet=TRUE,as.fun="data.table")

    addVars <- function(data){

        ## NDOSPERIOD
        data[,NDOSES:=cumsum(get(col.event)==TRUE),by=by]
        ## TDOS
        data[get(col.event)==TRUE,TDOS:=get(col.time)]
        data[,TDOS:=nafill(TDOS,type="locf"),by=by]
        ## TAD
        data[,TAD:=get(col.time)-TDOS]

    }
    
    data2 <- addVars(data2)
    
### If doses were expanded, we need to revert that
    doses <- data[get(col.event)==TRUE]

    doses <- addVars(doses)
    
    data3 <- rbind(doses
                  ,data2[get(col.event)!=TRUE]
                  ,fill=T)
    setorderv(data3,col.row.tmp)

    ## clean up
    data3[,(col.event):=NULL]
    data3[,(col.row.tmp):=NULL]
    
    setnames(data3,cc(TDOS,NDOSES,TAD),c(col.tdos,col.ndoses,col.tad))

    data3 <- as.fun(data3)

    return(data3)

}
