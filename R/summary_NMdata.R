##' print method for NMdata objects
##' @details The subjects are counted conditioned on the nmout
##'     column. If only id-level output tables are present, there are
##'     no nmout=TRUE rows. This means that in this case it will
##'     report that no IDs are found in output. The correct statement
##'     is that records are found for zero subjects in output tables.
##' @export
summary.NMdata <- function(data,...){
    

    if(!"NMdata"%in%class(data)) stop("data does not seem to be of class NMdata.")

    ## I need to look more into this. Some operations (merge?) drop
    ## many attributes but not the NMdata class. If that has happened,
    ## we ave nothing to use the class for.
    if(!"meta"%in%names(attributes(data))) {
        setattr(data,"class",setdiff(attr(data,"class"),"NMdata"))
        return(summary(data,...))
    }
    
    if(!is.data.table(data)) data <- as.data.table(data)
    ## derive how many subjects. Need to 

    
    s1 <- attr(data,"meta")
    s1$N.ids1 <- data[,.(N.ids=uniqueN(ID)),by="nmout"]

    N.ids.nmout <- s1$N.ids1[nmout==TRUE,N.ids]
    if(length(N.ids.nmout)==0) N.ids.nmout <- 0
    s1$N.ids <- rbind(
        data.table(NMOUT="From output tables",N.ids=N.ids.nmout)
       ,
        data.table(NMOUT="From input data only",N.ids=sum(
                                                    ! data[nmout==FALSE,unique(ID)] %in% data[nmout==TRUE,unique(ID)]
                                                )
                   )
    )
    s1$N.ids1 <- NULL

    s1$N.rows <- data[,.(N.rows=.N),by="nmout"]
    s1$N.evids <- NA
    if("EVID"%in%colnames(data)){
        s1$N.evids <- data[,.N,by=.(nmout,EVID)]
    }
    
    setattr(s1,"class",c("summary_NMdata",class(s1)))

    s1
}


##' @export
print.summary_NMdata <- function(x,...){

    
    if(!"summary_NMdata"%in%class(x)) stop("list does not seem to be of class NMdata")
    vars <- copy(x$variables)
    if(!is.data.table(vars)){
        vars <- as.data.table(vars)
    }
    
    tabs.out <- copy(x$tables)
    if(!is.data.table(tabs.out)){
        tabs.out <- as.data.table(tabs.out)
    }

    vars[,included:=!is.na(COLNUM)]
    vars <- mergeCheck(vars,data.table(included=c(TRUE,FALSE),
                                       ## inc=factor(c("included","not"),levels=c("included","not"))),
                                       inc=c("included","not")),
                       by="included")


    vars.sum <- vars[source!="NMscanData"][,.N,by=.(table,inc)]
    vars.sum1 <- dcast(vars.sum,table~inc,value.var="N")
    if("not"%in%colnames(vars.sum1)) vars.sum1[,not:=0]
    vars.sum1[,print.inc:=paste0(included,"/",sum(c(included,not),na.rm=T)),by=.(table)]


    
    ## include level
    tabs.out[,tabn:=1:.N]
    vars.sum2 <- mergeCheck(vars.sum1,tabs.out[,.(table=name,idlevel,tabn)],by="table",all.x=T)
    vars.sum2[,level:="row"]
    vars.sum2[idlevel==TRUE,level:="ID"]
    ## order as treated in NMscanData
    setorder(vars.sum2,tabn,na.last=TRUE)

    vars.sum2[,`:=`(tabn=NULL,idlevel=NULL,included=NULL,not=NULL)]
    setnames(vars.sum2,"print.inc","used/total")


    

#### other info to include. 
    dt.nmout <- data.table(nmout=c(TRUE,FALSE),NMOUT=c("From output tables","From input data only"))

    ## how many ids (broken down on output vs. input-only)
    
                                        #n1 <- merge(x$N.rows,x$N.ids,by="nmout")
    n2 <- melt(x$N.rows,id.vars="nmout",variable.name="N")
    n3 <- mergeCheck(n2,dt.nmout,by="nmout",all.x=TRUE)
    n4 <- dcast(n3,N~NMOUT,value.var="value")

    N.ids <- dcast(x$N.ids,.~NMOUT,value.var="N.ids")
    N.ids[,N:="N.ids"]
    N.ids[,.:=NULL]
    
    n5 <- rbind(n4,N.ids,fill=T)
    n5[is.na(n5)] <- 0
    

    ## model name
    cat("Model: ",x$model,"\n")
    
    cat("\nTables, number of columns in tables, and their detail level:\n")
    print(vars.sum2,row.names=FALSE)

    cat("\nNumbers of ID's and rows in data\n")
    print(n5,row.names=FALSE,...)

    
    if(any(!is.na(x$N.evids))){
        ## how many rows in output (broken down on EVID)

        ## if rows recovered, how many (broken down on EVID)
        evids1 <- mergeCheck(x$N.evids,dt.nmout,by="nmout",all.x=TRUE)
        
        evids2 <- dcast(evids1,EVID~NMOUT,value.var="N")
        evids2[is.na(evids2)] <- 0
        
        cat("\nDistribution of rows on event types\n")
        print(evids2,row.names=FALSE)
    }        

    return(invisible(NULL))

}
