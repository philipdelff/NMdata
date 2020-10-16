##' read input data and translate column names according to the $INPUT section
##' 
##' @description Based on a nonmem run (lst and/or mod file), this
##'     function finds the input data and reads it. But it reads it
##'     like the nonmem run by applying DROP/SKIP arguments and
##'     alternative naming of columns in the nonmem run.
##' @param file a .lst (output) or a .mod (input) control stream
##'     file. The filename does not need to end in .lst. It is
##'     recommended to use the output control stream because it
##'     reflects the model as it was run rather than how it is planned
##'     for next run. However, see file.mod and dir.data.
##' @param file.mod The input control stream file path. Default is to look for
##'     \"file\" with extension changed to .mod (PSN style). You can
##'     also supply the path to the file, or you can provide a
##'     function that translates the output file path to the input
##'     file path. If dir.data is missing, the input control stream is
##'     needed. This is because the .lst does not contain the path to
##'     the data file. The .mod file is only used for finding the data
##'     file. How to interpret the datafile is read from the .lst
##'     file. See dir.data too.
##' @param dir.data The data directory can only be read from the
##'     control stream (.mod) and not from the output file (.lst). So
##'     if you only have the output file, use dir.data to tell in
##'     which directory to find the data file. If dir.data is
##'     provided, the .mod file is not used at all.
##' @param use.rds If an rds file is found with the exact same name
##'     (except for .rds instead of say .csv) as the text file
##'     mentioned in the Nonmem control stream, should this be used
##'     instead? The default is yes, and NMwriteData will create this
##'     by default too.
##' @param applyFilters If TRUE (default), IGNORE and ACCEPT
##'     statements in the nonmem control streams are applied before
##'     returning the data.
##' @param quiet Default is to inform a little, but TRUE is useful for
##'     non-interactive stuff.
##' @param as.fun The default is to return data in data.tables. Pass a
##'     function in as.fun to convert to something else. If
##'     data.frames are wanted, use as.fun=as.data.frame. See
##'     ?runAsFun.
##' @param invert If TRUE, the data rows that are dismissed by the
##'     Nonmem data filters (ACCEPT and IGNORE) and only this will be
##'     returned. Only used if applyFilters is TRUE.
##' @details The line containing whom the license is issued to cannot
##'     be retrieved. Special characters like accents and the
##'     registerred trademark (R) sign are likely to cause trouble if
##'     locales are changed (say from a linux system running Nonmem to
##'     a Windows or Mac running R), so this line is
##'     discarded. Columns that are dropped (using DROP or SKIP in
##'     $INPUT) in the model will be included in the output.
##' @family DataRead
##' @export

NMtransInput <- function(file, use.rds=TRUE, file.mod=NULL,
                         dir.data=NULL, applyFilters=FALSE,
                         quiet=FALSE, invert=FALSE, as.fun=NULL) {
### the lst file only contains the name of the data file, not the path to it. So we need to find the .mod instead.

    if(missing(file)) file <- NULL
    file.find.data <- file

    if(!is.null(file.mod) && !is.null(dir.data)) {
        messageWrap("Both file.mod and dir.data are non-NULL. Not allowed.",
                    fun.msg=stop)
    }
    if(is.null(dir.data)) {

        file.find.data <- getFileMod(file.lst=file,file.mod=file.mod)
        
        if(!file.exists(file.find.data)) {
            messageWrap("control stream (.mod) not found. Default is to look next to .lst file. See argument file.mod if you want to look elsewhere. If you don't have a .mod file, see the dir.data argument. Input data not used.",fun.msg=warning)
        }

    }

    ## According to NM manual IV-1, $INPUT and $INFILE are the same thing.    
    lines <- NMgetSection(file,section="INPUT",keepName=F)
    if(is.null(lines)) {
        lines <- NMgetSection(file,section="INPT",keepName=F)
    }
    if(is.null(lines)) {stop("Could not find $INPUT or $INPT section in control stream. Cannot interpret data. Is file really the path to a valid nonmem control stream?")}

    ## get rid of redundant spaces
    line <- gsub(" +"," ",paste(lines,collapse=" "))
    line <- sub("^ ","",line)
    line <- sub(" $","",line)

    nms <- strsplit(line," ")[[1]]

### this is to keep even dropped columns
    nms <- sub("(.*) *= *(DROP|SKIP)","\\1",nms)
    nms <- sub(".*=(.*)","\\1",nms)


    ## get input data file name. Nonmem manual says:
###  The first character string appearing after $DATA is the name of the file
### containing the data. Since it is to be used in a FORTRAN OPEN statement,
### this name may not include embedded commas, semi-colons, parentheses, or
### spaces.
    lines.data <- NMgetSection(file.find.data,section="DATA",keepName=F,keepComments=F,keepEmpty=F)
    if(is.null(lines.data)) {
        lines.data <- NMgetSection(file.find.data,section="INFILE",keepName=F,keepComments=F,keepEmpty=F)
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
    if(use.rds && file.exists(path.data.input.rds)){
        if(!quiet) message("Read rds input data file.")
        path.data.input <- path.data.input.rds
        data.input <- readRDS(path.data.input)
    } else {
        if(file.exists(path.data.input)){
            if(!quiet) message("Read delimited text input data file.")
            data.input <- NMreadCsv(path.data.input,as.fun="none")
        } else {
            stop(paste("Input data file not found. Was expecting to find",path.data.input))
            ##        use.input <- FALSE
        }
    }

### filters must be applied here according to NM manual IV-1
    if(applyFilters){
        data.input <- NMtransFilters(data.input,file=file,invert=invert,quiet=quiet,as.fun="none")
    }

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
        if(any(duplicated(nms))){
            messageWrap(paste("Duplicated variable names declared in nonmem $INPUT section. Only first will be used:",paste(nms[duplicated(nms)],collapse=", ")),fun.msg=warning)
        }
        nms2 <- cnames.input[-(1:length(nms))]
        if(length(nms2)&&any(duplicated(nms2))){
            messageWrap(paste("Duplicated variable names detected in input data not processed by Nonmem. Only first will be used:",paste(nms2[duplicated(nms2)],collapse=", ")),fun.msg=warning)
        }
        nms.cross <- c(unique(nms),unique(nms2))
        if(any(duplicated(nms.cross))){
            messageWrap(paste("The same variable names are found in input variables as read by nonmem and the rest of input data file. Please look at column names in input data and the $INPUT section in nonmem control stream. Only the first occurrence of the columns will be used:",paste(unique(nms.cross[duplicated(nms.cross)]),collapse=", ")),fun.msg=warning)
        }

#### Reduce to unique column names
        
        data.input <- data.input[,unique(cnames.input),with=F]
        
    }

    data.input <- runAsFun(data.input,as.fun)

    return(data.input)
    

    
}
