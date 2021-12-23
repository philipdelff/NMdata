##' Extract the data file used in a control stream
##'
##' @param file The input control stream or the list file.
##' @param dir.data See NMscanInput. If used, only the file name
##'     mentioned in $DATA is used. dir.data will be used as the path,
##'     and the existence of the file in that directory is not
##'     checked.
##' @param file.mod The input control stream. Default is to look for
##'     \"file\" with extension changed to .mod (PSN style). You can
##'     also supply the path to the file, or you can provide a
##'     function that translates the output file path to the input
##'     file path. The default behavior can be configured using
##'     NMdataConf. See dir.data too.
##' @return The path to the input data file.
##' @export

NMextractDataFile <- function(file,dir.data=NULL,file.mod){

    if(missing(file.mod)) file.mod <- NULL
    file.mod <- NMdataDecideOption("file.mod",file.mod)

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
    
    ## get input data file name. Nonmem manual says:
###  The first character string appearing after $DATA is the name of the file
### containing the data. Since it is to be used in a FORTRAN OPEN statement,
### this name may not include embedded commas, semi-colons, parentheses, or
### spaces.
    lines.data <- NMreadSection(file,section="DATA",keepName=FALSE,keepComments=FALSE,keepEmpty=FALSE)
    if(is.null(lines.data)) {
        lines.data <- NMreadSection(file,section="INFILE",keepName=FALSE,keepComments=FALSE,keepEmpty=FALSE)
    }
    if(is.null(lines.data)) stop("Could not find $DATA or $INFILE section in nonmem model. Please check the lst file.")

    ## pick $DATA and the next string
    lines.data2 <- paste(lines.data,collapse=" ")
    string.path.data <- sub(" *([^ ]+) +.*","\\1",lines.data2)

    if(is.null(dir.data)) {
        pathIsAbs <- function(path) grepl("(^/|^[a-z]:/)",path,perl = TRUE)
        if(!pathIsAbs(string.path.data)) {
            path.data <- filePathSimple(dirname(file),string.path.data)
        }
    } else {
        path.data <- filePathSimple(dir.data,basename(string.path.data))
    }

    ## path.data.input.rds <- sub("^(.+)\\..+$","\\1.rds",path.data.input)
    path.data.rds <- fnExtension(path.data,".rds")

    exists.file <- file.exists(path.data)
    exists.file.rds <- file.exists(path.data.rds)

    return(list(
    DATA=lines.data
   ,string=string.path.data
   ,path=path.data
   ,path.rds=path.data.rds
   ,exists.file=exists.file
   ,exists.file.rds=exists.file.rds
    ))
    
}
