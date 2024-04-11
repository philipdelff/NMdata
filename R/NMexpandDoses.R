##' Transform repeated dosing events (ADDL/II) to individual dosing events
##'
##' Replaces single row repeated dosing events by multiple lines, then
##' reorders rows with respect to ID and TIME. If the row order is
##' different, you have to reorder the output manually.
##' 
##' @param data The data set to expand
##' @param col.time The name of the column holding the time on which
##'     time since previous dose will be based. This is typically
##'     actual or nominal time since first dose.
##' @param col.id The subject identifier. All new columns will be
##'     derived within unique values of this column.
##' @param col.evid The name of the event ID column. This must exist
##'     in data. Default is EVID.
##' @param track.expand Keep track of what rows were in data
##'     originally and which ones are added by NMexpandDoses by
##'     including a column called nmexpand? nmexpand will be TRUE if
##'     the row is "generated" by NMexpandDoses.
##' @param subset.dos A string that will be evaluated as a custom
##'     expression to identify relevant events. 
##' @param quiet Suppress messages back to user (default is FALSE)
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @return A data set with at least as many rows as data. If doses
##'     are found to expand, these will be added.
##' @import data.table
##' @export

NMexpandDoses <- function(data,col.time="TIME",col.id="ID",col.evid="EVID",track.expand=FALSE,subset.dos,quiet=FALSE,as.fun){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    ADDL <- NULL
    II <- NULL
    EVID <- NULL
    TIME <- NULL
    nmexpand <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

### data.was.dt is used to making as.fun="data.table" default if data was dt.
    data.was.dt <- TRUE
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)
        data.was.dt <- FALSE
    }

    if(missing(as.fun)) as.fun <- NULL
    if(data.was.dt && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    
    if(missing(subset.dos)) subset.dos <- NULL
    col.subset.dos <- tmpcol(data,base="tmp.subset.dos",prefer.plain=TRUE)
    if(is.null(subset.dos)){
        data[,(col.subset.dos):=TRUE]
    } else {
        data[eval(parse(text=subset.dos)),(col.subset.dos):=TRUE]
    }

    ## copy must be before testing if anything to do. If not, a
    ## reference to original data is returned which is not intended.
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)
    }    

    if(!all(cc(ADDL,II)%in%colnames(data))){
        if(!quiet) message("ADDL and II not found in data. Nothing done.")
        if(track.expand) data[,nmexpand:=FALSE]
        data[,(col.subset.dos):=NULL]
        return(data)
    }

    if(!col.id%in%colnames(data)){
        stop("col.id must refer to a name of an existing column in data.")
    }
    if(!col.time%in%colnames(data)){
        stop("col.time must refer to a name of an existing column in data.")
    }

    
    if(data[get(col.subset.dos)==TRUE&
            get(col.evid)%in%c(1,4)&
            !is.na(ADDL)&
            ADDL>0,.N]==0){
        if(!quiet) message("No dosing events with ADDL>0 found. Nothing to be done.")
        if(track.expand) data[,nmexpand:=FALSE]
        data[,(col.subset.dos):=NULL]
        return(data)
    }

    rec.tmp <- tmpcol(data)
    data[,(rec.tmp):=.I]
    
    recs.folded <- data[get(col.subset.dos)==TRUE&
                        get(col.evid)%in%c(1,4)&
                        !is.na(ADDL)&ADDL>0,get(rec.tmp)]
    
    if(any(recs.folded[II==0|is.na(II)])) {
        warning("II values of zero found in events to be expanded. Is this an error?")
    }
    if(any(recs.folded[II%%1]!=0)) {
        warning("II seem to contain non-integers. Is this an error?")
    }
    if(any(recs.folded[ADDL%%1]!=0)) {
        warning("II seem to contain non-integers. Is this an error?")
    }
    

    
    
    newtimes <- data[get(rec.tmp)%in%recs.folded,
                     .(TIME=seq(get(col.time),by=II,length.out=ADDL+1)
                      ,ADDL=0
                      ,II=0
                      ,nmexpand=c(FALSE,rep(TRUE,ADDL)))
                    ,by=rec.tmp]
    setnames(newtimes,"TIME",col.time)
    data.merge <- copy(data)

    
    
    data.merge[,(col.time):=NULL][
       ,ADDL:=NULL][
       ,II:=NULL]
    newdoses <- mergeCheck(newtimes,data.merge,by=rec.tmp,quiet=TRUE)
    if(track.expand) { 
    ##     newdoses[,nmexpand:=TRUE]
        data[,nmexpand:=FALSE]
    } else {
        newdoses[,nmexpand:=NULL]
    }
    
    ## rbind
    newdat <- rbind(data[!get(rec.tmp)%in%recs.folded],newdoses)

    ## setorder - remember ID and groups
    setorderv(newdat,c(col.id,col.time,rec.tmp))

    ## discard tmp columns
    newdat[,(rec.tmp):=NULL]
    
    newdat[,(col.subset.dos):=NULL]

    newdat <- as.fun(newdat)
    
    return(newdat)
}
