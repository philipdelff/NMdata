##' @keywords internal

searchColRow <- function(file,dir.data,file.data,translate.input,use.rds,args.fread,col.id,tab.row){

    dia <- suppressWarnings(NMscanInput(file,file.mod=file.mod
                                       ,dir.data=dir.data
                                       ,file.data=file.data
                                       ,quiet=TRUE
                                       ,translate=translate.input
                                       ,use.rds=use.rds
                                       ,applyFilters=FALSE
                                       ,args.fread=args.fread
                                       ,details=TRUE
                                       ,col.id=col.id
                                       ,as.fun="data.table"))
    
    cols.row.input <- colnames(dia)[dia[,unlist(lapply(.SD,function(x)uniqueN(x)==.N))]]

    cols.row.output <- colnames(tab.row)[tab.row[,unlist(lapply(.SD,function(x)uniqueN(x)==.N))]]

    cols.row.both <- intersect(cols.row.input,cols.row.output)
### we should not merge on these even if unique
    cols.row.both <- setdiff(cols.row.both,c("AMT","DV","TIME"))
    if(length(cols.row.both)){
        
        msg0 <- paste("\nInput data columns will be appended to output data. However, column(s) were identified as unique identifiers, present in both input and output data. If this column or one of these columns is not modified by the Nonmem run, consider using this in col.row for a robust merge of input and output data. Candidate columns:",paste(cols.row.both,collapse=", "))
    } else if(length(cols.row.input)) {
        msg0 <- paste("\nInput data columns will be appended to output data. However, column(s) were identified as unique identifiers, present in input data. If this column or one of these columns is not modified by the Nonmem run, consider adding it to a row-level output table and using this in col.row for a robust merge of input and output data. Candidate columns:",paste(cols.row.input,collapse=", "))
    } else {
        msg0 <- "Input and output data were searched for candidate unique row identifiers. None found."
    }
    
    msg0
}
