##' Transform repeated dosing events (ADDL/II) to individual dosing events
##'
##' Replaces single row repeated dosing events by multiple lines, then
##' reorders rows with respect to ID and TIME. If the row order is
##' different, you have to reorder the output manually.
##' 
##' @param data The data set to expand
##' @param quiet Suppress messages back to user (default is FALSE)
##' @param col.time The name of the column holding the time on which
##'     time since previous dose will be based. This is typically
##'     actual or nominal time since first dose.
##' @param col.id The subject identifier. All new columns will be
##'     derived within unique values of this column.
##' @param col.evid The name of the event ID column. This must exist
##'     in data. Default is EVID.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @return A data set with at least as many rows as data. If doses
##'     are found to expand, these will be added.
##' @import data.table
##' @export

NMexpandDoses <- function(data,col.time="TIME",col.id="ID",col.evid="EVID",quiet=FALSE,as.fun){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    ADDL <- NULL
    II <- NULL
    EVID <- NULL
    TIME <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    ## copy must be before testing if anything to do. If not, a
    ## reference to original data is returned which is not intended.
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)   
    }    


    if(!all(cc(ADDL,II)%in%colnames(data))){
        if(!quiet) message("ADDL and II not found in data. Nothing done.")
        return(data)
    }

    rec.tmp <- tmpcol(data)
    data[,(rec.tmp):=.I]
    
    recs.folded <- data[get(col.evid)%in%c(1,4)&!is.na(ADDL)&ADDL>0,get(rec.tmp)]
    newtimes <- data[get(rec.tmp)%in%recs.folded,
                     .(TIME=seq(get(col.time),by=II,length.out=ADDL+1)
                      ,ADDL=0
                      ,II=0)
                    ,by=rec.tmp]
    setnames(newtimes,"TIME",col.time)
    data.merge <- copy(data)

    
    
    data.merge[,(col.time):=NULL][
       ,ADDL:=NULL][
       ,II:=NULL]
    newdoses <- mergeCheck(newtimes,data.merge,by=rec.tmp,quiet=TRUE)

    ## rbind
    newdat <- rbind(data[!get(rec.tmp)%in%recs.folded],newdoses)

    ## setorder - remember ID and groups
    setorderv(newdat,c(col.id,col.time,rec.tmp))

    ## discard tmp columns
    newdat[,(rec.tmp):=NULL]

    newdat <- as.fun(newdat)
    
    return(newdat)
}
