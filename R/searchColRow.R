##' Check row identifier in a model for necessary properties.
##'
##' This function is only meant for internal use by NMscanData. 
##' @param file a .lst (output) or a .mod (input) control stream
##'     file. The filename does not need to end in .lst. It is
##'     recommended to use the output control stream because it
##'     reflects the model as it was run rather than how it is planned
##'     for next run. However, see file.mod and dir.data.
##' @param file.mod The input control stream file path. Default is to
##'     look for \"file\" with extension changed to .mod (PSN
##'     style). You can also supply the path to the file, or you can
##'     provide a function that translates the output file path to the
##'     input file path. If dir.data is missing, the input control
##'     stream is needed. This is because the .lst does not contain
##'     the path to the data file. The .mod file is only used for
##'     finding the data file. How to interpret the datafile is read
##'     from the .lst file. The default can be configured using
##'     NMdataConf. See dir.data too.
##' @param dir.data The data directory can only be read from the
##'     control stream (.mod) and not from the output file (.lst). So
##'     if you only have the output file, use dir.data to tell in
##'     which directory to find the data file. If dir.data is
##'     provided, the .mod file is not used at all.
##' @param file.data Specification of the data file path. When this is
##'     used, the control streams are not used at all.
##' @param use.rds If an rds file is found with the exact same name
##'     (except for .rds instead of say .csv) as the text file
##'     mentioned in the Nonmem control stream, should this be used
##'     instead? The default is yes, and NMwriteData will create this
##'     by default too.
##' @param applyFilters If TRUE (default), IGNORE and ACCEPT
##'     statements in the Nonmem control streams are applied before
##'     returning the data.
##' @param translate.input If TRUE (default), data columns are named
##'     as interpreted by Nonmem (in $INPUT). If data file contains
##'     more columns than mentioned in $INPUT, these will be named as
##'     in data file (if data file contains named variables).
##' @param recover.cols recover columns that were not used in the
##'     Nonmem control stream? Default is TRUE. Can only be negative
##'     when translate=FALSE.
##' @param details If TRUE, metadata is added to output. In this case,
##'     you get a list. Typically, this is mostly useful if
##'     programming up functions which behavior must depend on
##'     properties of the output. See details.
##' @param col.id The name of the subject ID column. Optional and only
##'     used to calculate number of subjects in data. Default is
##'     modified by NMdataConf.
##' @param col.row The name of the row counter column. Optional and
##'     only used to check whether the row counter is in the data.
##' @param quiet Default is to inform a little, but TRUE is useful for
##'     non-interactive stuff.
##' @param args.fread List of arguments passed to fread. Notice that
##'     except for "input" and "file", you need to supply all
##'     arguments to fread if you use this argument. Default values
##'     can be configured using NMdataConf.
##' @return A character message about the findings if any
##' @keywords internal

searchColRow <- function(file,file.mod=file.mod,dir.data,file.data,translate.input,use.rds,args.fread,col.id,tab.row){

    dia <- suppressWarnings(NMscanInput(file
                                       ,file.mod=file.mod
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
