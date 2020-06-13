##' read input data and translate names according to the $INPUT section
##' 
##' @description Based on a nonmem run, this function finds the input
##'     data and reads it. But it reads it like the nonmem run by
##'     applying DROP arguments and alternative naming of columns in
##'     the nonmem run.
##' @param file a .lst or a .mod. If dir.data is missing, the .mod is
##'     required to exist next to the .lst file. This is because the
##'     .lst does not contain the path to the data file. The .mod file
##'     is only used for finding the data file. How to interpret the
##'     datafile is read from the .lst file.
##' @param useRDS If an rds file is found with the exact same name
##'     (except for .rds instead of say .csv) as the text file
##'     mentioned in the Nonmem control stream, should this be used
##'     instead? The default is yes, and NMwriteData will create this
##'     by default too.
##' @param dir.data The data directory can only be read from the
##'     control stream (.mod) and not from the output file (.lst). So
##'     if you only have the output file, use dir.data to tell in
##'     which directory to find the data file. If dir.data is
##'     provided, the .mod file is not used at all.
##' @param quiet Default is to inform a little, but TRUE is useful for
##'     non-interactive stuff.
##' @param as.dt Return a data.table? data.table is default, if not a
##'     data.frame is returned.
##' @param debug start by running browser()?
##' @details The line containing whom the license is issued to cannot
##'     be retrieved. Special characters like accents and the
##'     registerred trademark (R) sign are likely to cause trouble if
##'     locales are changed (say from a linux system running Nonmem to
##'     a Windows or Mac running R), so this line is discarded.
##' @family Nonmem
##' @export

## implement NULL,  RECORDS or at least say it's not done, use col.row instead.

NMtransInput <- function(file,useRDS=TRUE,dir.data,applyFilters=FALSE,quiet=FALSE,as.dt=TRUE,debug=F) {

    if(debug) browser()
    
### the lst file only contains the name of the data file, not the path to it. So we need to find the .mod instead.
    file.find.data <- file

    if(missing(dir.data)) dir.data <- NULL
    if(is.null(dir.data)) {
        file.find.data <- sub("\\.lst$","\\.mod",file)
    }

## According to NM manual IV-1, $INPUT and $INFILE are the same thing.    
    lines <- NMgetSection(file,section="INPUT",keepName=F)
    if(is.null(lines)) {
        lines <- NMgetSection(file,section="INFILE",keepName=F)
    }
    if(is.null(lines)) {stop("Could not find $INPUT or $INFILE section in control stream. Cannot interpret data. Is file really the path to a valid nonmem control stream?")}

    ## get rid of redundant spaces
    line <- gsub(" +"," ",paste(lines,collapse=" "))
    line <- sub("^ ","",line)
    line <- sub(" $","",line)

    nms <- strsplit(line," ")[[1]]

    ## find all equals. drop or rename?
    ## drop <- grepl("= *DROP",nms)
### this is if they are not intended to be kept.
    ## nms <- sub(".*=(.*)","\\1",nms)
    ## nms[nms=="DROP"] <- paste0("DROP",1:sum(nms=="DROP"))
### this is to keep even dropped columns
    nms <- sub("(.*) *= *DROP","\\1",nms)
    nms <- sub(".*=(.*)","\\1",nms)


    ## get input data file name. Nonmem manual says:
###  The first character string appearing after $DATA is the name of the file
### containing the data. Since it is to be used in a FORTRAN OPEN statement,
### this name may not include embedded commas, semi-colons, parentheses, or
### spaces.
    lines.data <- NMgetSection(file.find.data,section="DATA",keepName=F,keepComments=F,keepEmpty=F)

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
    if(useRDS && file.exists(path.data.input.rds)){
        if(!quiet) message("found rds input file. This will be used.")
        path.data.input <- path.data.input.rds
        data.input <- readRDS(path.data.input)
    } else {
        if(file.exists(path.data.input)){
            if(!quiet) message("Found input data file. Reading with NMreadCsv")
            data.input <- NMreadCsv(path.data.input)
        } else {
            stop(paste("Input data file not found. Was expecting to find",path.data.input))
            ##        use.input <- FALSE
        }
    }

### filters must be applied here according to NM manual IV-1
    if(applyFilters){
        data.input <- NMtransFilters(data.input,file=file,quiet=quiet)
    }

    cnames.input <- colnames(data.input)
    cnames.input[1:length(nms)] <- nms
    colnames(data.input) <- cnames.input


    if(as.dt) {
        as.data.table(data.input)
    } else {
        as.data.frame(data.input)
    }

}
