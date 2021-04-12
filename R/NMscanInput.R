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
##' @family DataRead
##' @export

NMscanInput <- function(file, use.rds, file.mod,
                        dir.data=NULL, applyFilters=FALSE, translate=TRUE,
                        details=FALSE, col.id="ID", quiet, invert=FALSE,
                        as.fun) {
    

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    nid <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
### the lst file only contains the name of the data file, not the path to it. So we need to find the .mod instead.

    if(missing(file)) file <- NULL
    file.find.data <- file
    if(missing(as.fun)) as.fun <- NULL
    if(missing(file.mod)) file.mod <- NULL
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(use.rds)) use.rds <- NULL
    use.rds <- NMdataDecideOption("use.rds",use.rds)    
    
    if(!is.null(file.mod) && !is.null(dir.data)) {
        messageWrap("Both file.mod and dir.data are non-NULL. Not allowed.",
                    fun.msg=stop)
    }
    if(is.null(dir.data)) {
        
        file.mod <- NMdataDecideOption("file.mod",file.mod)

        file.find.data <- file.mod(file)
        ## file.find.data <- getFileMod(file.lst=file,file.mod=file.mod)
        
        if(!file.exists(file.find.data)) {
            messageWrap("control stream (.mod) not found. Default is to look next to .lst file. See argument file.mod if you want to look elsewhere. If you don't have a .mod file, see the dir.data argument. Input data not used.",fun.msg=warning)
        }

    }

    ## According to NM manual IV-1, $INPUT and $INFILE are the same thing.    
    lines <- NMreadSection(file,section="INPUT",keepName=FALSE,keepComments=FALSE,cleanSpaces=TRUE)
    if(is.null(lines)) {
        lines <- NMreadSection(file,section="INPT",keepName=FALSE,keepComments=FALSE,cleanSpaces=TRUE)
    }
    if(is.null(lines)) {stop("Could not find $INPUT or $INPT section in control stream. Cannot interpret data. Is file really the path to a valid nonmem control stream?")}
    
    ## get rid of redundant spaces
    line <- gsub(" +"," ",paste(lines,collapse=" "))
    line <- sub("^ ","",line)
    line <- sub(" $","",line)

### nms is the names of columns as in nonmem control stream
    nms <- strsplit(line," ")[[1]]

### this is to keep even dropped columns
    nms <- sub("(.*) *= *(DROP|SKIP)","\\1",nms)
    ## For now, we just take the first name used in A=B labeling. 
    nms <- sub(".*=(.*)","\\1",nms)


    ## get input data file name. Nonmem manual says:
###  The first character string appearing after $DATA is the name of the file
### containing the data. Since it is to be used in a FORTRAN OPEN statement,
### this name may not include embedded commas, semi-colons, parentheses, or
### spaces.
    lines.data <- NMreadSection(file.find.data,section="DATA",keepName=F,keepComments=F,keepEmpty=F)
    if(is.null(lines.data)) {
        lines.data <- NMreadSection(file.find.data,section="INFILE",keepName=F,keepComments=F,keepEmpty=F)
    }
    if(is.null(lines.data)) stop("Could not find $DATA or $INFILE section in nonmem model. Please check the lst file.")

    ## pick $DATA and the next string
    lines.data2 <- paste(lines.data,collapse=" ")
    path.data.input <- sub(" *([^ ]+) +.*","\\1",lines.data2)

    if(is.null(dir.data)) {
        pathIsAbs <- function(path) grepl("(^/|^[a-z]:/)",path,perl = TRUE)
        if(!pathIsAbs(path.data.input)) {
            path.data.input <- filePathSimple(dirname(file),path.data.input)
        }
    } else {
        path.data.input <- filePathSimple(dir.data,basename(path.data.input))
    }

    path.data.input.rds <- sub("^(.+)\\..+$","\\1.rds",path.data.input)
    type.file <- NA_character_
    if(use.rds && file.exists(path.data.input.rds)){
        type.file <- "rds"
        if(!quiet) message("Read rds input data file.")
        path.data.input <- path.data.input.rds
        data.input <- as.data.table(readRDS(path.data.input))
    } else {
        if(file.exists(path.data.input)){
            type.file <- "text"
            if(!quiet) message("Read delimited text input data file.")
            data.input <- NMreadCsv(path.data.input,as.fun=identity)
        } else {
            stop(paste("Input data file not found. Was expecting to find",path.data.input))
        }
    }

### filters must be applied here according to NM manual IV-1
    if(applyFilters){
        data.input <- NMtransFilters(data.input,file=file,invert=invert,quiet=quiet,as.fun=identity)
    }

    if(translate){
### cnames.input is the names of columns as in input data file
        cnames.input <- colnames(data.input)
        
        ## More column names can be specified in the nonmem control stream
        ## than actually found in the input data. We will simply disregard
        ## them.
        if(length(nms)>length(cnames.input)){
            nms <- nms[1:length(cnames.input)]
            messageWrap("More column names specified in Nonmem $INPUT than found in data file. The additional names have been disregarded.",fun.msg=warning)
        }
        
        cnames.input[1:length(nms)] <- nms
        colnames(data.input) <- cnames.input

        ## check for unique column names
        if(any(duplicated(cnames.input))) {
            nms2 <- cnames.input[-(1:length(nms))]
            if(any(duplicated(nms))){
                messageWrap(paste("Duplicated variable names declared in nonmem $INPUT section. Only first will be used:",paste(nms[duplicated(nms)],collapse=", ")),fun.msg=warning)
                ## nms.u <- unique(nms)
            } 
            if(length(nms2)&&any(duplicated(nms2))){
                messageWrap(paste("Duplicated variable names detected in input data not processed by Nonmem. Only first will be used:",paste(nms2[duplicated(nms2)],collapse=", ")),fun.msg=warning)
                ## nms2.u <- unique(nms2)
            }
            nms.cross <- c(unique(nms),unique(nms2))
            if(any(duplicated(nms.cross))){
                
                messageWrap(paste("The same variable names are found in input variables as read by nonmem and the rest of input data file. Please look at column names in input data and the $INPUT section in nonmem control stream. Only the first occurrence of the columns will be used:",paste(unique(nms.cross[duplicated(nms.cross)]),collapse=", ")),fun.msg=warning)
            }
#### Reduce to unique column names
            
            data.input <- data.input[,unique(cnames.input),with=F]
            
        }
    }

    as.fun <- NMdataDecideOption("as.fun",as.fun)
    


    if(details){

        meta <- data.table(
            file=path.data.input,
            name=basename(path.data.input),
            filetype=type.file,
            nrow=nrow(data.input),
            ncol=ncol(data.input),
            file.mtime=file.mtime(path.data.input)
        )
        if(col.id%in%cnames.input) {
            
            meta$nid <-
                data.input[,uniqueN(get(col.id))]
        }
        
        data.input <- as.fun(data.input)
        return(list(data=data.input,meta=meta))
    }
    
    data.input <- as.fun(data.input)
    return(data.input)
}
