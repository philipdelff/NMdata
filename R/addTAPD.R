##' Add time since previous dose to data, time of previous dose, most
##' recent dose amount, cumulative number of doses, and cumulative
##' dose amount.
##' 
##'
##' For now, doses have to be in data as EVID=1 and/or EVID=4
##' records. They can be in the format of one row per dose or repeated
##' dosing notation using ADDL and II.
##' @param data The data set to add the variables to.
##' @param col.time Name of time column (created by addTAPD). Default
##'     it TIME.
##' @param col.tpdos Name of the time of previous dose column (created
##'     by addTAPD). Default is TPDOS. Set to NULL to not create this
##'     column.
##' @param col.tapd Name of the time of previous dose column (created
##'     by addTAPD). Default is TAPD. Set to NULL to not create this
##'     column.
##' @param col.evid The name of the event ID column. This must exist
##'     in data. Default is EVID.
##' @param col.amt col.evid The name of the dose amount column. This
##'     must exist in data. Default is AMT.
##' @param col.pdosamt The name of the column to be created holding
##'     the previous dose amount. Set to NULL to not create this
##'     column.
##' @param col.doscuma The name of the column to be created holding
##'     the cumulative dose amount. Set to NULL to not create this
##'     column.
##' @param col.doscumn The name of the column (created by addTAPD) that
##'     holds the cumulative number of doses administered to the
##'     subject. Set to NULL to not create this column.
##' @param prefix.cols String to be prepended to all generated column
##'     names, that is each of col.tpdos, col.tapd, col.ndoses,
##'     col.pdosamt, col.doscuma that are not NULL.
##' @param suffix.cols String to be appended to all generated column
##'     names, that is each of col.tpdos, col.tapd, col.ndoses,
##'     col.pdosamt, col.doscuma that are not NULL.
##' @param subset.dos A string that will be evaluated as a custom
##'     expression to identify relevant events. See subset.is.complete
##'     as well.
##' @param subset.is.complete Only used in combination with
##'     non-missing subset.dos. By default, subset.dos is used in
##'     addition to the impact of col.evid (must be 1 or 4) and col.amt (greater than zero). If
##'     subset.is.complete=TRUE, subset.dos is used alone, and
##'     col.evid and col.amt are completely ignored. This is typically
##'     useful if the events are not doses but other events that are
##'     not expressed as a typical dose combination of EVID and AMT
##'     columns.
##' @param order.evid Order of events. This will only matter if there
##'     are simultaneous events of different event types within
##'     subjects. Typically if using nominal time, it may be important
##'     to specify whether samples at dosing times are pre-dose
##'     samples. The default is c(3,0,4,1,2) - i.e. samples and
##'     simulations are pre-dose. See details.
##' @param by Columns to do calculations within. Default is ID.
##' @param SDOS Scaling value for columns related to dose amount,
##'     relative to AMT values. col.pdosamt and col.doscuma are
##'     affected and will be derived as AMT/SDOSE.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param col.ndoses Deprecated. Use col.doscumn instead.
##' @details addTAPD does not require the data to be ordered, and it
##'     will not order it. This means you can run addTAPD before
##'     ordering data (which may be one of the final steps) in data
##'     set preparation. The argument called order.evid is important
##'     because of this. If a dosing event and a sample occur at the
##'     same time, when which dose was the previous for that sample?
##'     Default is to assume the sample is a pre-dose sample, and
##'     hence output will be calculated in relation to the dose
##'     before. If no dose event is found before, NA's will be
##'     assigned.
##' @return A data.frame with additional columns
##' @import data.table
##' @export
##' @family DataCreate


addTAPD <- function(data,col.time="TIME",col.evid="EVID",col.amt="AMT",col.tpdos="TPDOS",col.tapd="TAPD",col.pdosamt="PDOSAMT",col.doscuma="DOSCUMA",col.doscumn="DOSCUMN",prefix.cols,suffix.cols,subset.dos,subset.is.complete,order.evid = c(3,0,2,4,1),by="ID",SDOS=1,as.fun,col.ndoses){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    nmexpand <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    args <- getArgs()
    deprecatedArg("col.ndoses","col.doscumn",args=args)
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(missing(subset.dos)) subset.dos <- NULL
    
    if(!missing(subset.is.complete)&&is.null(subset.dos)) {
        messageWrap("subset.is.complete can only be used in combination with subset.dos.",fun.msg=stop)
    }
    if(missing(subset.is.complete)) subset.is.complete <- FALSE
    
    subset.event.0 <- sprintf("%s%%in%%c(1,4)&%s>0",col.evid,col.amt)
    if(subset.is.complete) {
        subset.event <- subset.dos
    } else if(!is.null(subset.dos)) {
        
        subset.event <- paste(subset.dos,subset.event.0,sep="&")
    } else {
        subset.event <- subset.event.0
    }
    if(missing(prefix.cols)) prefix.cols <- NULL
    if(missing(suffix.cols)) suffix.cols <- NULL
    
    if(is.data.table(data)){
        data <- copy(data)
    } else {
        data <- as.data.table(data)   
    }    

    
    args.create.optional <- cc(col.tpdos,col.tapd,col.doscumn,col.pdosamt,col.doscuma)
    if(!is.null(prefix.cols) || !is.null(suffix.cols)){
        for(col in args.create.optional){
            if(!is.null(get(col))){
                assign(col,paste0(prefix.cols,get(col),suffix.cols))
            }
        }
    }


    ## report if columns will be overwriten
    cols.exist <- intersect(colnames(data),c(col.tpdos,col.tapd,col.doscumn,col.pdosamt,col.doscuma))
    if(length(cols.exist)){
        messageWrap(paste0("Columns will be overwritten: ",paste(cols.exist,collapse=", ")),fun.msg=warning)
    }

    ## row identifier for reordering data back to original order after modifications
    col.row.tmp <- tmpcol(data,base="row")
    data[,(col.row.tmp):=.I]

    col.event <- tmpcol(data,base="event")
    data[,(col.event):=FALSE]
    data[eval(parse(text=subset.event)),(col.event):=TRUE]

    
    
    ## expand doses if necessary
    data2 <- NMexpandDoses(data=data,col.id=by,quiet=TRUE,as.fun="data.table",col.time=col.time,col.evid=col.evid,track.expand=TRUE,subset.dos=subset.dos)
    
    col.evidorder <- tmpcol(data2,base="evidorder")
    data2[,(col.evidorder):=match(get(col.evid),table=order.evid)]

    setorderv(data2,c(by,col.time,col.evidorder))

    col.tpdos.tmp <- tmpcol(data2,base="tpdos.tmp")

    addVars <- function(data){
        ## NDOSPERIOD
        if(!is.null(col.doscumn)){
            data[!is.na(get(col.time)),(col.doscumn):=cumsum(get(col.event)==TRUE),by=by]
        }
        ## TPDOS - time of previous dose - needed for TAPD
        data[get(col.event)==TRUE,(col.tpdos.tmp):=get(col.time)]
        data[,(col.tpdos.tmp):=nafill(get(col.tpdos.tmp),type="locf"),by=by]
        ## Relative time since previous dose
        if(!is.null(col.tapd)){
            data[,(col.tapd):=get(col.time)-get(col.tpdos.tmp)]
        }
        ## previous dose amount
        if(!is.null(col.pdosamt)){
            col.pdosamt.tmp <- tmpcol(data,base="tmpcol.pdosamt")
            data[,(col.pdosamt.tmp):=NA_real_]
            ## data[!is.na(get(col.amt))&get(col.amt)>0,(col.pdosamt.tmp):=get(col.amt)]
            data[get(col.event)==TRUE,(col.pdosamt.tmp):=get(col.amt)]
            data[,(col.pdosamt):=nafill(get(col.pdosamt.tmp),type="locf")/SDOS,by=by]
            data[,(col.pdosamt.tmp):=NULL]
        }
        ## DOSCUMA - Cumulative Amount of Dose Received
        if(!is.null(col.doscuma)){
            data[get(col.event)==TRUE&!is.na(get(col.amt)),(col.doscuma):=cumsum(get(col.amt))/SDOS,by=by]
            data[,(col.doscuma):=nafill(c(0,get(col.doscuma)),type="locf")[-1],by=by]
        }
        ## clean up tpdos
        if(is.null(col.tpdos)){
            data[,(col.tpdos.tmp):=NULL]
        } else {
            if(col.tpdos%in%colnames(data)) data[,(col.tpdos):=NULL]
            setnames(data,col.tpdos.tmp,col.tpdos)
        }
        invisible(data)
    }

    ## data2 <- addVars(data2)
    addVars(data2)

    
### this should not be based on col.event. 
    
### If doses were expanded, we need to revert that
    doses <- data[get(col.event)==TRUE]
    ## doses <- addVars(doses)
    addVars(doses)

    
    data3 <- rbind(
        doses
       ,
        ## data2[get(col.event)!=TRUE&nmexpand==FALSE]
        data2[get(col.event)!=TRUE&nmexpand==FALSE]
       ,fill=T)
    setorderv(data3,col.row.tmp)

    ## clean up
    data3[,(col.event):=NULL]
    data3[,(col.row.tmp):=NULL]
    data3[,(col.evidorder):=NULL]
    data3[,nmexpand:=NULL]


    data3 <- as.fun(data3)

    return(data3)

}
