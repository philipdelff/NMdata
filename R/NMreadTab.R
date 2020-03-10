##' Read a table file from NONMEM 
##'
##' @param file path to NONMEM table file
##' @param silent logical stating whether or not information is printed.
##' @param debug Start by calling browser()?
##' @param ... Arguments passed to fread.
##' @return Nonmem table as df.
##' @import data.table
##' @family Nonmem
##' @export


NMreadTab <- function(file,silent=F,...,debug=F) {

    if(debug) browser()


#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    TABLE <- NULL
    TABLENO <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    ## arg checks
    if(!is.character(file)) stop("file should be a character string",call.=F)
    if(!file.exists(file)) stop("argument file is not a path to an existing file.",call.=F)

    if(!silent){
        message("Reading data using fread")
    }
    dt1 <- fread(file,fill=T,header=T,skip=1,...)

    cnames <- colnames(dt1)
    if(!silent){
        message("Adding table numbers to data")
    }
    ## find table numbers
    dt1[grep("^TABLE.*",as.character(get(cnames[1])),invert=F,perl=T),TABLE:=get(cnames[1])]
    dt1[,TABLENO:=cumsum(!is.na(TABLE))+1]
    dt1[,TABLE:=NULL]
    if(!silent){
        message("getting rid of non-data rows")
    }
    dt1 <- dt1[grep("^ *[[:alpha:]]",as.character(get(cnames[1])),invert=T,perl=T)]

    cols.dup <- duplicated(colnames(dt1))
    if(any(cols.dup)){
        warning(paste0("Duplicated column names found: ",paste(colnames(dt1)[cols.dup],collapse=","),". Cleaning."),immediate.=TRUE)
        dt1 <- dt1[,unique(cnames),with=FALSE]
    }

    if(!silent){
        message("Making sure everything is numeric")
    }

    cnames <- colnames(dt1)
    for (col in cnames) {
        set(dt1, j=col, value=as.numeric(dt1[[col]]))
    }
    
    
    return(dt1)
}
