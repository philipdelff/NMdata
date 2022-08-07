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
##' @param col.evid The name of the event ID column. This must exist in data. Default is EVID.
##' @param by Columns to do calculations within. Default is ID. 
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @import data.table
##' @details This is still experimental.
##' @export


addTAD <- function(data,col.time="TIME",col.tdos="TDOS",col.tad="TAD",col.ndoses="NDOSES",col.evid="EVID",by="ID",as.fun){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    NDOSES <- NULL
    TDOS <- NULL
    TAD <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)


    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)   
    }    

    ## row identifier for reordering data back to original order after modifications
    col.row.tmp <- tmpcol(data)
    data[,(col.row.tmp):=.I]
    
### quit if no doses found etc
    


    
    ## expand doses if necessary
    data2 <- NMexpandDoses(data=data,quiet=TRUE,as.fun="data.table")

    addVars <- function(data){
        ## NDOSPERIOD
        data[,NDOSES:=cumsum(get(col.evid)%in%c(1,4)),by=by]
        ## TDOS
        data[get(col.evid)%in%c(1,4),TDOS:=get(col.time)]
        data[,TDOS:=nafill(TDOS,type="locf"),by=by]
        ## TAD
        data[,TAD:=get(col.time)-TDOS]
    }
    
    data2 <- addVars(data2)
    
### If doses were expanded, we need to revert that
    doses <- data[get(col.evid)%in%c(1,4)]

    doses <- addVars(doses)
    
    data3 <- rbind(doses
                  ,data2[!get(col.evid)%in%c(1,4)]
                  ,fill=T)
    setorderv(data3,col.row.tmp)
    data3[,(col.row.tmp):=NULL]
    
    setnames(data3,cc(TDOS,NDOSES,TAD),c(col.tdos,col.ndoses,col.tad))

    data3 <- as.fun(data3)

    return(data3)

}
