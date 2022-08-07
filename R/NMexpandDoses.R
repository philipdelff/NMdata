##' Transform repeated dosing events (ADDL/II) to individual dosing events
##'
##' Replaces single row repeated dosing events by multiple lines, then
##' reorders rows with respect to ID and TIME. If the row order is
##' different, you have to reorder the output manually.
##' 
##' @param data The data set to expand
##' @param quiet Suppress messages back to user (default is FALSE)
##' @return A data set with at least as many rows as data. If doses
##'     are found to expand, these will be added.
##' @import data.table
##' @details This is still experimental. 
##' @export

NMexpandDoses <- function(data,quiet=FALSE){

    if(!all(cc(ADDL,II)%in%colnames(data))){
        if(!quiet) message("ADDL and II not found in data. Nothing done.")
        return(data)
    }

    data <- copy(data)
    rec.tmp <- NMdata:::tmpcol(data)
    data[,(rec.tmp):=.I]
    
    recs.folded <- data[EVID==1&!is.na(ADDL)&ADDL>0,get(rec.tmp)]
    newtimes <- data[get(rec.tmp)%in%recs.folded,
                     .(TIME=seq(TIME,by=II,length.out=ADDL+1))
                    ,by=rec.tmp]
    newdoses <- mergeCheck(newtimes,data[,!("TIME")],by=rec.tmp,quiet=T)

    ## rbind
    newdat <- rbind(data[!get(rec.tmp)%in%recs.folded],newdoses)

    ## setorder - remember ID and groups
    setorderv(newdat,c("ID","TIME",rec.tmp))

    ## discard tmp columns
    newdat[,rec.tmp:=NULL]
    
    return(newdat)
}
