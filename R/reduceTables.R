##' reduce tables from NMscanTables to fewer objects
##' @param tables Object from NMtables
##' @param col.nmout The name of the column holding logical of whether
##'     row was found in output tables.
##' @import data.table
##' @details This function is under development. It is not polished
##'     for other than internal use by NMscanData. It will likely
##'     return a different format in the future.
##' @return A list of data.tables: One row-level, one id-level data
##'     set and their column specifications.
##' @keywords internal


reduceTables <- function(tables,col.nmout){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####
    
    included <- NULL
    level <- NULL
    scope <- NULL
    variable <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    meta.output <- copy(NMinfoDT(tables)$tables)

    
    rows.flo <- meta.output[scope=="firstlastonly"]
    if(rows.flo[,.N]>0) {
        warning("One or more output tables with FIRSTLASTONLY option detected. This is not supported, and the table will be disregarded. Use a combination of NMscanTables, NMscanInput, and merge manually.")
        k <- meta.output[,which(scope=="firstlastonly")]
        tables <- tables[-k]
        meta.output <- meta.output[-k]
    }

    tab.row <- NULL
    dt.vars <- NULL

    
    if(meta.output[level=="row",.N]) {
        
        ## there might be a little bit to save by reducing the columns before cbind.
        tab.row <- Reduce(cbind,tables[which(meta.output$level=="row")])
### get all names from this and then select unique to get a table with the included variables
        list.vars <- lapply(tables[which(meta.output$level=="row")],names)
        list.vars <- lapply(list.vars,as.data.table)
        list.vars <- lapply(seq_along(list.vars),function(n)list.vars[[n]][,file:=names(list.vars)[n]])
        dt.vars1 <- rbindlist(list.vars)
        setnames(dt.vars1,c("V1"),"variable")
        ## notice the selection of names in dt.vars and tab.row must be identical
        dt.vars1[,included:=!duplicated(variable)]
        dt.vars1[,`:=`(source="output",level="row")]
        
        tab.row <- tab.row[,!duplicated(colnames(tab.row)),with=FALSE]

        tab.row[,(col.nmout):=TRUE]

        dt.vars <- rbind(dt.vars,dt.vars1)
        
        dt.vars <- rbind(dt.vars,
                         data.table(variable=col.nmout
                                   ,file=NA_character_
                                   ,included=TRUE 
                                   ,source="NMscanData"
                                   ,level="row"
                                    ))
    }


### combine idlevel tables into one
    tab.idlevel <- NULL
    dt.vars.id <- NULL
    if(any(meta.output$level=="id")) {
        
        tab.idlevel <- Reduce(cbind,tables[which(meta.output$level=="id")])
        tab.idlevel <- tab.idlevel[,unique(colnames(tab.idlevel)),with=FALSE]

### get all names from this and then select unique to get a table with the included variables
        list.vars.id <- lapply(tables[which(meta.output$level=="id")],names)
        list.vars.id <- lapply(list.vars.id,as.data.table)
        list.vars.id <- lapply(seq_along(list.vars.id),function(n)list.vars.id[[n]][,file:=names(list.vars.id)[n]])
        dt.vars.id <- rbindlist(list.vars.id)
        setnames(dt.vars.id,c("V1"),"variable")
        dt.vars.id[,included:=!duplicated(variable)]
        ## notice the selection of names in dt.vars.id and tab.row must be identical
        ## dt.vars.id[,included:=!duplicated(variable)]
        dt.vars.id[,`:=`(source="output",level="id")]
    }
    
    ## tab.row and tab.idlevel should have dt.vars and dt.vars.id as
    ## NMinfo data. And maybe they should be merged? 
    list(tab.row=tab.row,tab.idlevel=tab.idlevel,dt.vars=dt.vars,dt.vars.id=dt.vars.id)
}
