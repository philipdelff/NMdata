##' Find and read input data and optionally translate column names
##' according to the $INPUT section
##'
##' This function finds and reads the input data based on a control
##' stream file path. It can align the column names to the definitions
##' in $INPUT in the control stream, and it can subset the data based
##' on ACCEPT/IGNORE statements in $DATA. I supports a few other ways
##' to identify the input data file than reading the control stream,
##' and it can also read an rds file instead of the delimited text
##' file used by Nonmem.
##' 
##' @description Based on a Nonmem run (lst and/or mod file), this
##'     function finds the input data and reads it. It reads the data
##'     like the Nonmem run by applying DROP/SKIP arguments and
##'     alternative naming of columns in the Nonmem run.
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
##' @param translate If TRUE (default), data columns are named as
##'     interpreted by Nonmem (in $INPUT). If data file contains more
##'     columns than mentioned in $INPUT, these will be named as in
##'     data file (if data file contains named variables).
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
##' @param col.row The name of the row counter column. Optional and only
##'     used to check whether the row counter is in the data.
##' @param quiet Default is to inform a little, but TRUE is useful for
##'     non-interactive stuff.
##' @param args.fread List of arguments passed to fread. Notice that
##'     except for "input" and "file", you need to supply all
##'     arguments to fread if you use this argument. Default values
##'     can be configured using NMdataConf.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param invert If TRUE, the data rows that are dismissed by the
##'     Nonmem data filters (ACCEPT and IGNORE) and only this will be
##'     returned. Only used if applyFilters is TRUE.
##' @details Columns that are dropped (using DROP or SKIP in $INPUT)
##'     in the model will be included in the output.
##'
##' It may not work if a column is dropped, and a new column is
##' renamed to the same name. Say you have DV and CONC as the only two
##' columns (not possible but illustrative), and in Nonmem you do
##' DV=DROP DV. Not sure it will work in Nonmem, and it probably won't
##' work in NMscanInput.
##'
##' @return A data set, class defined by 'as.fun'
##' 
##' @family DataRead
##' @export

NMscanInput <- function(file, use.rds, file.mod, dir.data=NULL,
                        file.data=NULL, applyFilters=FALSE,
                        translate=TRUE,recover.cols=TRUE,
                        details=TRUE, col.id="ID", col.row, quiet,
                        args.fread, invert=FALSE, as.fun) {
    
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    datafile <- NULL
    nid <- NULL
    input <- NULL
    result <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    
### the lst file only contains the name of the data file, not the path
### to it. So we need to find the .mod instead.

    if(missing(file)) {
        messageWrap("file is needed. If you want to use input control stream only, use that as file and ignore the file.mod argument.",fun.msg=stop)
    }
    if(!file.exists(file)){
        stop("file has to be a valid path to an existing file.")
    }
    file.find.data <- file
    if(missing(as.fun)) as.fun <- NULL
    if(missing(file.mod)) file.mod <- NULL

    if(missing(col.row)) {
        col.row <- NULL
    } 
    col.row <- NMdataDecideOption("col.row",col.row)

    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(use.rds)) use.rds <- NULL
    use.rds <- NMdataDecideOption("use.rds",use.rds)    
    if(missing(args.fread)) args.fread <- NULL
    args.fread <- NMdataDecideOption("args.fread",args.fread)
    

        
    ## identify the data file name and additional info
    info.datafile <- NMextractDataFile(file=file.find.data,dir.data,file.mod=file.mod,file.data=file.data)
    
    type.file <- NA_character_
    if(use.rds && info.datafile$exists.file.rds){
        type.file <- "rds"
        if(!quiet) message(paste0("Read rds input data file:\n",info.datafile$path.rds,"."))
        path.data.input <- info.datafile$path.rds
        data.input <- as.data.table(readRDS(path.data.input))
    } else {
        if(file.exists(info.datafile$path)){
            type.file <- "text"
            if(!quiet) message(paste0("Read delimited text input data file:\n",info.datafile$path,"."))
            path.data.input <- info.datafile$path
            data.input <- NMreadCsv(path.data.input,args.fread=args.fread,as.fun="data.table")
        } else {
            stop(paste("Input data file not found. Was expecting to find",info.datafile$path))
        }
    }
    
    ## keeping a backup before translating column names and filtering
    ## rows. This is used for very litle which should be done here
    ## instead of making a deep copy.
    data.input.0 <- copy(data.input)

    ### not used
    ## nminfo.input.0 <- NMinfoDT(data.input)

    
    
### filters must be applied here according to NM manual IV-1. They are applied before translating column names.
    if(applyFilters){
        
        data.input <- NMapplyFilters(data.input,file=file,invert=invert,quiet=quiet,as.fun="data.table")
    }

    
### cnames.input is the names of columns as in input data file
    data.input <- NMtransInp(data.input,file,translate=translate,recover.cols=recover.cols)
    
    col.id.inp <- col.id
    if(translate){
        col.id.inp <- NMinfoDT(data.input,"input.colnames")[result==col.id,datafile][1]
    }

    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    if(details){
        
        meta <- list()
        meta$datafile <- info.datafile
        input.create.time <- NMinfo(data.input)$dataCreate$CreationTime
        if(is.null(input.create.time)) input.create.time <- NA

        meta$tables <- data.table(
            source="input",
            file=path.data.input,
            file.mtime=file.mtime(path.data.input),
            file.logtime=input.create.time,
            filetype=type.file,
            name=basename(path.data.input),
            nrow=nrow(data.input.0),
            ncol=ncol(data.input.0),
            nid=NA_real_
        )
        
        meta <- append(meta,NMinfoDT(data.input))

        meta$tables$has.col.row <- NA
        if(!is.null(col.row)){
            meta$tables$has.col.row <- col.row%in%meta$input.colnames[,result]
        }
        meta$tables$has.col.id <- NA
        if(!is.null(col.id)){
            meta$tables$has.col.id <- col.id%in%meta$input.colnames[,result]
        }

        setcolorder(meta$tables,intersect(c("source","name","nrow","ncol","firstonly","lastonly","firstlastonly","format","sep","nid","idlevel","has.row","maxLength","full.length","filetype","file.mtime","file.logtime","file"),colnames(meta$tables)))

        
        if(col.id%in%NMinfoDT(data.input,"input.colnames")[,result]) {
            meta$tables[,nid:=data.input.0[,uniqueN(get(col.id.inp))]]
        }

        data.input <- as.fun(data.input)
        writeNMinfo(data.input,meta,byRef=TRUE)
        return(data.input)
    } else {
        data.input <- as.fun(data.input)
    }
    
    return(data.input)
}
