##' Check data for Nonmem compatibility
##'
##' Under development. Check data in various ways for compatibility
##' with Nonmem. Some findings will be reported even if they will not
##' make Nonmem fail but because they are typical dataset issues.
##' 
##' @param data The data to check
##' @param col.id The name of the column that holds the subject
##'     identifier. Default is "ID".
##' @param col.time The name of the column holding actual time
##' @param col.flagn Optionally, the name of the column holding numeric exclusion flags.
##' @param col.row 
##' @param debug Start by calling browser()?
##' @details The following checks are performed. The term "numeric" does not refer to a numeric representation in R, but compatibility with Nonmem. The character string "2" is in this sense a valid numeric, "id2" is not.
##' \itemize{
##' \item If an exclusion flag is used (for ACCEPT/IGNORE in Nonmem), elements must be non-missing and integers. If an exclusion flag is found, the rest of the checks are performed on rows where that flag equals 0 (zero) only.
##' \item col.time (TIME), EVID, ID, CMT, MDV: If present, elements must be non-missing and numeric.
##' \item col.time (TIME) must be non-negative
##' \item EVID must be in {0,1,2,3,4}
##' \item CMT must be positive integers
##' \item MDV must be the binary (1/0) representation of is.na(DV)
##' \item AMT must be 0 or NA for EVID 0 and 2
##' \item AMT must be positive for EVID 1 and 4
##' \item DV must be numeric
##' \item DV must be missing for EVID in {1,4}.
##' \item ID must be positive and values cannot be disjoint (all records for each ID must be following each other. This is technically not a requirement in Nonmem but most often an error. Use a second ID column if you deliberately want to soften this check)
##' \item TIME cannot be decreasing within ID, unless EVID in {3,4}.
##' \item all ID's must have doses (EVID in {1,4})
##' \item all ID's must have observations (EVID==0)
##' }
##' @import NMdata

##' @import data.table NMdata

### TODO

## We check for NA's by a numeric conversion. But what if the NA comes from a value of say "b" that is not a valid NA representation. This has to be addressed before/at NMasNumeric.

## We need several potentially nested checks on several colums:
## Example: col.time has to be present, numeric, non-negative. These
## should be done one after another for readibility of code.

## col.row if supplied
### check for uniqueness, integer, (increasing)
### use col.row instead of tmprow

## convert to data.table

## checks for NM compatibility.
### Report which can be used and which cannot.
### error if EVID, DV, AMT etc are not numeric to Nonmem

## run flagsAssign to summarize all findings? 

## column names

### col.flagn
### subset to col.flagn==0

#### checks of specific columns - row level
### TIME
### CMT
### AMT - todo
### DV
### MDV

#### ID-level checks
### increasing time within subjects and EVID=3 or 4
### subjects without obs
### subjects without doses

#### format results


NMcheckData <- function(data,col.id="ID",col.time="TIME",col.flagn=NULL,col.row=NULL,debug=F){
    if(debug) browser()
    data <- copy(as.data.table(data))

### TODO: check flags for NA's before subsetting on FLAG
    
    row <- tmpcol(data,base="ROW",prefer.plain=TRUE)
    data[,(row):=.I]

    NMasNumeric <- function(x,warn=F) {
        if(warn){
            as.numeric(as.character(x))
        } else {
            suppressWarnings(as.numeric(as.character(x)))
        }
    }

    ##' listEvents is for row-level findings
    ##' @param col is the actual column to be used for the condition
    ##' @param name 
    ##' @param fun if fun does not return TRUE, we have a finding.
    ##' @param colname is the column name reported to user.

    ##' @param new.rows.only. For nested criteria. Say that CMT is not
    ##' numeric for row 10. Then don't report that it is not an integer
    ##' too.
    listEvents <- function(col,name,fun,colname=col,dat=data,events=NULL,invert=FALSE,new.rows.only=T,debug=F){
        if(debug) browser()

        if(!col%in%colnames(dat)){
            if(events[check=="Column not found"&column==colname&level=="column",.N]==0){
                events <- rbind(events,
                                data.table(check="Column not found",column=colname,level="column")
                                ,fill=TRUE) }
            return(events)
        }
        
        if(invert){
            row <- dat[fun(get(col))==TRUE,get(row)]
        } else {
            row <- dat[fun(get(col))!=TRUE,get(row)]
        }

        if(length(row)==0) {
            res <- data.table(check=name,column=colname,row=NA,level="row")[0]
        } else {
            
            if(new.rows.only&&!is.null(events)){
                row <- setdiff(row,events[column==colname,row])
            }
            res <- data.table(check=name,column=colname,row=row,level="row")
        }
        rbind(events,res,fill=TRUE)
    }

    findings <- data.table(check="is NA",column="TIME",row=0,level="row")[0]

    if(!is.null(col.flagn)){
        findings <- listEvents(col.flagn,"Missing value",
                               fun=is.na,invert=T,events=findings,debug=FALSE)
        findings <- listEvents(col.flagn,"Not numeric",
                               fun=NMisNumeric,events=findings,
                               new.rows.only=T)
        findings <- listEvents(col.flagn,"col.flagn not an integer",
                               fun=function(x)x%%1==0,events=findings,
                               new.rows.only=T)
        if(col.flagn%in%colnames(data)){
### Done checking flagn. For rest of checks, only consider data where col.flagn==0
            data[,(col.flagn):=NMasNumeric(get(col.flagn))]
            if(is.numeric(data[,get(col.flagn)])){
                data <- data[get(col.flagn)==0]
            }
        }
    }
    
    
    cols.num <- c()
    
### Others that: If column present, must be numeric, and values must be non-NA. Remember eg DV and AMT can be NA.
    cols.num.if.avail <- c(col.time,"EVID","ID","CMT","MDV")
    for(col in cols.num.if.avail){
        if(!is.null(col)&&col%in%colnames(data)){
            cols.num <- c(cols.num,col)
        } else {
            findings <- rbind(findings,
                              data.table(check="Column not found",column=col,level="column"),
                              fill=TRUE)
        }
    }
### check for missing in cols.num
    ## listEvents("CMT",listEvents,name="is NA",fun=is.na,invert=TRUE,new.rows.only=T,
    ##            debug=TRUE)
    newfinds <- rbindlist( lapply(cols.num,listEvents,name="is NA",fun=is.na,invert=TRUE,new.rows.only=T,debug=FALSE) )
    findings <- rbind(findings,
                      newfinds
                     ,fill=TRUE)

### check for  non-numeric in cols.num
    newfinds <- rbindlist( lapply(cols.num,listEvents,name="Not numeric",fun=NMisNumeric,new.rows.only=T) )
    findings <- rbind(findings,
                      newfinds
                     ,fill=TRUE)

    ## a new row counter for internal use only. It matches the data we
    ## are checking but not the data supplied by user.
    rowint <- tmpcol(data,base="ROWINT",prefer.plain=TRUE)
    data[,(rowint):=.I]

    
##### overwrite cols.num with NMasNumeric of cols.num
    data[,(cols.num):=lapply(.SD,NMasNumeric),.SDcols=cols.num]


### TIME must be positive
    findings <- listEvents(col.time,"Negative time",fun=function(x)x>=0,events=findings,debug=F)

### EVID must be in c(0,1,2,3,4)
    findings <- listEvents("EVID","EVID in 0:4",function(x) x%in%c(0:4),events=findings)

    
### CMT must be a positive integer
    ## findings <- listEvents("CMT","Missing value",
    ##                        fun=is.na,events=findings)
    ## findings <- listEvents("CMT","Not numeric",
    ##                        fun=NMisNumeric,events=findings,
    ##                        new.rows.only=T)
    findings <- listEvents("CMT","CMT not a positive integer",
                           fun=function(x)x>0&x%%1==0,events=findings,
                           new.rows.only=T)

### ID must be a positive integer
    findings <- listEvents("ID","ID not a positive integer",
                           fun=function(x)x>0&x%%1==0,events=findings,
                           new.rows.only=T)


### MDV should perfectly reflect is.na(DV)
    if("MDV"%in%colnames(data)){
        ## browser()
        data[,MDVDV:=!is.na(MDV)&MDV==as.numeric(is.na(DV))]
        findings <- listEvents("MDVDV","MDV does not match DV",colname="MDV",fun=function(x)x==TRUE,events=findings)
    }


###### DV
### DV must be present
### DV must be numeric for EVID==0
    findings <- listEvents("DV","DV not numeric",fun=is.na,events=findings,invert=TRUE,dat=data[EVID%in%c(0)])
### DV should be NA for dosing records
    findings <- listEvents("DV","DV not NA in dosing recs",fun=is.na,events=findings,dat=data[EVID%in%c(1,4)])

### Requirements to DV for EVID==2 and EVID==3?

#### AMT
    ## positive for EVID 1 and 4
    findings <- listEvents("AMT","Non-positive dose amounts",
                           fun=function(x)x>=0,events=findings,
                           dat=data[EVID%in%c(1,4)])
    ## must be 0 or NA for EVID 0 and 2
    findings <- listEvents("AMT","Non-zero dose amounts for obs or sim record",
                           fun=function(x)is.na(x)||(is.numeric(x)&&x==0),
                           events=findings,
                           dat=data[EVID%in%c(0,2)])
    
    ## ID-level checks
### Warning if the same ID is in non-consequtive rows
    data[,ID.jump:=c(0,diff(get(rowint))),by=col.id]
    findings <- listEvents("ID.jump",colname="ID",name="ID disjoint",fun=function(x) x<=1,events=findings)

### within ID, time must be increasing. Unless EVID%in% c(3,4) or events are jumped
    data[,newID:=ID]
    if(col.time%in%colnames(data)){
        
        data[,isnewID:=get(col.id)!=shift(get(col.id),n=1)]
        data[1,isnewID:=TRUE]
        data[,reset:=EVID%in%c(3,4)]
        data[,newID:=cumsum(as.numeric(isnewID)+as.numeric(reset))]
        data[,checkTimeInc:=c(TRUE,diff(get(col.time))>=0),by=.(newID)]

        findings <- listEvents(col="checkTimeInc",name="Time decreasing",fun=function(x) x==TRUE,
                               colname="TIME",events=findings)
    }
    
    data[,Nrep:=.N,by=c("newID","CMT","EVID",col.time)]
    findings <- listEvents(col="Nrep",name="Duplicated event",function(x) x<2,colname="ID, CMT, EVID, TIME",events=findings)

### subjects without doses
    all.ids <- data[,unique(get(col.id))]
    tab.evid.id <- data[,.N,by=c(col.id,"EVID")]
    ids.no.doses <- setdiff(all.ids,tab.evid.id[EVID%in%c(1,4),get(col.id)])

    if(length(ids.no.doses)){
        findings <- rbind(findings
                         ,data.table(check="Subject has no doses",column="EVID",ID=ids.no.doses,level="ID")
                         ,fill=TRUE)
    }
### subjects without observations    
    ids.no.obs <- setdiff(all.ids,tab.evid.id[EVID%in%c(0),get(col.id)])
    if(length(ids.no.obs)){
        findings <- rbind(findings
                         ,
                          data.table(check="Subject has no obs",column="EVID",ID=ids.no.obs,level="ID")
                         ,fill=TRUE)
    }

### Add ID's to row-level findings
    if(!is.null(col.id)){
        findings.row <- findings[level=="row"]
        if(col.id%in%colnames(findings.row)) findings.row[,(col.id):=NULL]
        findings.row <- mergeCheck(findings.row,data[,c(row,col.id),with=F],by.x="row",by.y=row,all.x=T,fun.commoncols=stop)
        
        findings <- rbind(findings[level!="row"],findings.row,fill=T)
    }

### use the row identifier for reporting
    if(!is.null(col.row)){
        setcolnames(findings,"row",row)
        findings <- mergeCheck(findings,data[,c(row,col.row),with=F],by=row,all.x=T,fun.commoncols=stop)
        findings[,(row):=NULL]
    }
    setcolorder(findings,c("row","ID","column","check"))

    if(nrow(findings)==0) {
        message("No findings. Great!")
        invisible(findings)
    } else {
        print(findings[,.N,by=.(column,check)],row.names=FALSE)
        cat("\n")
        return(invisible(findings))
    }

}
