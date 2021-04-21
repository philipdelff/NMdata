##' Extract the data file used in a control stream
##'
##' @param file The input control stream or the list file.
##' @param dir.data See NMscanInput. If used, only the file name
##'     mentioned in $DATA is used. This file name is looked for in
##'     dir.data. If dir.data is NULL (default), the path is either
##'     absolute (only unix paths starting in / are supported.

### To be used internally so far - don't export

NMextractDataFile <- function(file,dir.data=NULL){
    ## get input data file name. Nonmem manual says:
###  The first character string appearing after $DATA is the name of the file
### containing the data. Since it is to be used in a FORTRAN OPEN statement,
### this name may not include embedded commas, semi-colons, parentheses, or
### spaces.
    lines.data <- NMreadSection(file,section="DATA",keepName=F,keepComments=F,keepEmpty=F)
    if(is.null(lines.data)) {
        lines.data <- NMreadSection(file,section="INFILE",keepName=F,keepComments=F,keepEmpty=F)
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

    ## path.data.input.rds <- sub("^(.+)\\..+$","\\1.rds",path.data.input)
    path.data.input.rds <- fnExtension(path.data.input,".rds")

    exists.file <- file.exists(path.data.input)
    exists.file.rds <- file.exists(path.data.input.rds)

    return(list(path=path.data.input
               ,path.rds=path.data.input.rds
               ,exists.file=exists.file
               ,exists.file.rds=exists.file.rds))

}
