##' Extract the data file used in a control stream
##'
##' A function that identifies the input data file based on a control
##' stream. The default is to look at the $DATA section of of the
##' output control stream (or input control stream if file.mod
##' argument is used). This can be partly or fully overruled by using
##' the dir.data or file.data arguments.
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
##' @param file.data Specification of the data file path. When this is
##'     used, the control streams are not used at all.
##' @return The path to the input data file.
##' @export

NMextractDataFile <- function(file,dir.data=NULL,file.mod,file.data=NULL){
    
    if(missing(file.mod)) file.mod <- NULL
    file.mod <- NMdataDecideOption("file.mod",file.mod)

    file.data <- NMdataDecideOption("file.data",file.data)
    if(is.character(file.data)&&file.data=="extract") {
        file.data <- NULL
    } else {
        file.data <- file.data(file)
    }
    
    lines.data <- NULL
    string.file.data <- NULL

    if(is.null(file.data)) {
        file <- file.mod(file)

        if(!file.exists(file)) {
            messageWrap("Input control stream (typically .mod) not found. Default is to look next to .lst file. See argument file.mod if you want to look elsewhere. If you don't have a .mod file, see the dir.data argument. Input data not used.",fun.msg=warning)
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
### remove leading blanks, then only use string until first blank
        ## doesnt remove leading blanks as supposed to so splitting in two
        ## string.file.data <- sub("^ *([^ ]+) +.*$","\\1",lines.data2)
        string.file.data <- sub("^ +","",lines.data2)
        string.file.data <- sub(" .*","",string.file.data)

        if(is.null(dir.data)) {
            pathIsAbs <- function(path) grepl("(^/|^[a-z]:/)",path,perl = TRUE)
            if(!pathIsAbs(string.file.data)) {
                file.data <- filePathSimple(dirname(file),string.file.data)
            } else {
                file.data <- filePathSimple(string.file.data)
            }
        } else {
            file.data <- filePathSimple(dir.data,basename(string.file.data))
        }

    } 

#### apply dir.data if supplied
    if(!is.null(dir.data)){
        file.data <- file.path(dir.data,basename(file.data))
    }

    ## file.data.input.rds <- sub("^(.+)\\..+$","\\1.rds",file.data.input)
    file.data.rds <- fnExtension(file.data,".rds")

    exists.file <- file.exists(file.data)
    exists.file.rds <- file.exists(file.data.rds)

    return(list(
        DATA=lines.data
       ,string=string.file.data
       ,path=file.data
       ,path.rds=file.data.rds
       ,exists.file=exists.file
       ,exists.file.rds=exists.file.rds
    ))
    
}
