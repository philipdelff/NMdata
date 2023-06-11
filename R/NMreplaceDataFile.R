##' Replace data file used in Nonmem control stream
##'
##' @param file.mod
##' @param path.data
##' @param newfile
##'
##' @export

NMreplaceDataFile <- function(files,file.pattern,dir,path.data,newfile=file.mod,...){

    if(missing(files)) files <- NULL
    if(missing(dir)) dir <- NULL
    if(missing(file.pattern)) file.pattern <- NULL

    all.files <- getFilePaths(files=files,file.pattern=file.pattern,dir=dir,quiet=quiet)
    
    ## newfile
    ## if(!missing(newfile)&&!is.null(newfile) && length(all.files)>1) {
    ##     stop("if multiple files are edited, newfile must be missing or NULL.")
    ## }

    is.missing.newfile <- missing(newfile)
    
    res <- lapply(files,function(file.mod){
        if(is.missing.newfile) newfile <- file.mod
        extr.data <- NMextractDataFile(file.mod)
        sec.data.new <- paste("$DATA",sub(extr.data$string,path.data,extr.data$DATA,fixed=TRUE))
        NMwriteSection(files=files,section="DATA",newlines=sec.data.new,newfile=newfile,...)
    })
    

    invisible(res)
}
