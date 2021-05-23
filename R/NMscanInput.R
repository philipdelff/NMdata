##' read input data and translate column names according to the $INPUT section
##' 
##' @description Based on a nonmem run (lst and/or mod file), this function
##'     finds the input data and reads it. It reads the data like the nonmem run
##'     by applying DROP/SKIP arguments and alternative naming of columns in the
##'     nonmem run.
##' @param file a .lst (output) or a .mod (input) control stream file. The
##'     filename does not need to end in .lst. It is recommended to use the
##'     output control stream because it reflects the model as it was run rather
##'     than how it is planned for next run. However, see file.mod and dir.data.
##' @param file.mod The input control stream file path. Default is to look for
##'     \"file\" with extension changed to .mod (PSN style). You can also supply
##'     the path to the file, or you can provide a function that translates the
##'     output file path to the input file path. If dir.data is missing, the
##'     input control stream is needed. This is because the .lst does not
##'     contain the path to the data file. The .mod file is only used for
##'     finding the data file. How to interpret the datafile is read from the
##'     .lst file. The default can be configured using NMdataConf. See dir.data
##'     too.
##' @param dir.data The data directory can only be read from the control stream
##'     (.mod) and not from the output file (.lst). So if you only have the
##'     output file, use dir.data to tell in which directory to find the data
##'     file. If dir.data is provided, the .mod file is not used at all.
##' @param use.rds If an rds file is found with the exact same name (except for
##'     .rds instead of say .csv) as the text file mentioned in the Nonmem
##'     control stream, should this be used instead? The default is yes, and
##'     NMwriteData will create this by default too.
##' @param applyFilters If TRUE (default), IGNORE and ACCEPT statements in the
##'     nonmem control streams are applied before returning the data.
##' @param translate If TRUE (default), data columns are named as interpreted by
##'     Nonmem (in $INPUT). If data file contains more columns than mentioned in
##'     $INPUT, these will be named as in data file (if data file contains named
##'     variables).
##' @param details If TRUE, metadata is added to output. In this case, you get a
##'     list. Typically, this is mostly useful if programming up functions which
##'     behavior must depend on properties of the output. See details.
##' @param col.id The name of the subject ID column. Only used if details=TRUE
##'     to summarize number of subjects in data.
##' @param quiet Default is to inform a little, but TRUE is useful for
##'     non-interactive stuff.
##' @param args.fread List of arguments passed to fread. Notice that
##'     except for "input" and "file", you need to supply all
##'     arguments to fread if you use this argument. Default values
##'     can be configured using NMdataConf.
##' @param as.fun The default is to return data as a data.frame. Pass a function
##'     (say tibble::as_tibble) in as.fun to convert to something else. If
##'     data.tables are wanted, use as.fun="data.table". The default can be
##'     configured using NMdataConf.
##' @param invert If TRUE, the data rows that are dismissed by the Nonmem data
##'     filters (ACCEPT and IGNORE) and only this will be returned. Only used if
##'     applyFilters is TRUE.
##' @details The line containing whom the license is issued to cannot be
##'     retrieved. Special characters like accents and the registerred trademark
##'     (R) sign are likely to cause trouble if locales are changed (say from a
##'     linux system running Nonmem to a Windows or Mac running R), so this line
##'     is discarded. Columns that are dropped (using DROP or SKIP in $INPUT) in
##'     the model will be included in the output.
##'
##' It may not work if a column is dropped, and a new column is
##' renamed to the same name. Say you have DV and CONC as the only two
##' columns (not possible but illustrative), and in Nonmem you do
##' DV=DROP DV. Not sure it will work in Nonmem, and it probably won't
##' work in NMscanInput.
##' 
##' @family DataRead
##' @export

NMscanInput <- function(file, use.rds, file.mod,
                        dir.data=NULL, applyFilters=FALSE, translate=TRUE,
                        details=FALSE, col.id="ID", quiet, args.fread,
                        invert=FALSE,
                        as.fun) {
    

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    nid <- NULL
    result.all <- NULL
    input <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
### the lst file only contains the name of the data file, not the path to it. So we need to find the .mod instead.

    if(missing(file)) {
        stop("file is needed. If you want to use input control stream only, use that as file and ignore the file.mod argument.")
    }
    if(!file.exists(file)){
        stop("file has to be a valid path to an existing file.")
    }
    file.find.data <- file
    if(missing(as.fun)) as.fun <- NULL
    if(missing(file.mod)) file.mod <- NULL
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(use.rds)) use.rds <- NULL
    use.rds <- NMdataDecideOption("use.rds",use.rds)    
    if(missing(args.fread)) args.fread <- NULL
    args.fread <- NMdataDecideOption("args.fread",args.fread)
    
    if(!is.null(file.mod) && !is.null(dir.data)) {
        messageWrap("Both file.mod and dir.data are non-NULL. Not allowed.",
                    fun.msg=stop)
    }
    if(is.null(dir.data)) {
        
        file.mod <- NMdataDecideOption("file.mod",file.mod)
        file.find.data <- file.mod(file)

        if(!file.exists(file.find.data)) {
            messageWrap("control stream (.mod) not found. Default is to look next to .lst file. See argument file.mod if you want to look elsewhere. If you don't have a .mod file, see the dir.data argument. Input data not used.",fun.msg=warning)
        }

    }


    ## identify the data file name and additional info
    info.datafile <- NMextractDataFile(file=file.find.data,dir.data)
    
    type.file <- NA_character_
    if(use.rds && info.datafile$exists.file.rds){
        type.file <- "rds"
        if(!quiet) message("Read rds input data file.")
        path.data.input <- info.datafile$path.rds
        data.input <- as.data.table(readRDS(path.data.input))
    } else {
        if(file.exists(info.datafile$path)){
            type.file <- "text"
            if(!quiet) message("Read delimited text input data file.")
            path.data.input <- info.datafile$path
            data.input <- NMreadCsv(path.data.input,args.fread=args.fread,as.fun="data.table")
        } else {
            stop(paste("Input data file not found. Was expecting to find",info.datafile$path))
        }
    }
    data.input.0 <- copy(data.input)
    
### filters must be applied here according to NM manual IV-1. They are applied before translating column names.
    if(applyFilters){
        data.input <- NMapplyFilters(data.input,file=file,invert=invert,quiet=quiet,as.fun=identity)
    }

    if(translate){
### cnames.input is the names of columns as in input data file
        data.input <- NMtransInp(data.input,file)
        dt.colnames <- data.input$dt.colnames
        data.input <- data.input$data
    }

    
    col.id.inp <- col.id
    if(translate){
        col.id.inp <- dt.colnames[result.all==col.id,input][1]
    }
    
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    
    if(details){

        meta <- data.table(
            file=path.data.input,
            file.mtime=file.mtime(path.data.input),
            filetype=type.file,
            name=basename(path.data.input),
            nrow=nrow(data.input.0),
            ncol=ncol(data.input.0),
            nid=NA_real_
        )
        if(col.id%in%dt.colnames[,result.all]) {
            meta$nid <- data.input.0[,uniqueN(get(col.id.inp))]
        }
        
        data.input <- as.fun(data.input)
        return(list(data=data.input,meta=meta))
    }
    
    data.input <- as.fun(data.input)
    return(data.input)
}
