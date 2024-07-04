##' Replace data file used in Nonmem control stream
##'
##' @param files Paths to input control streams to modify. See
##'     file.pattern and dir too.
##' @param file.pattern A pattern to look for if `dir` is supplied too
##'     (and not `file.mod`). This is used to modify multiple input
##'     control streams at once.
##' @param dir Directory in which to look for `file.pattern`. Notice,
##'     use either just `file.mod` or both `dir` and `file.pattern`.
##' @param path.data Path to input control stream to use in newfile
##' @param newfile A path to a new control stream to write to (and
##'     don't edit contents of `file.mod`). Default is to overwrite `file.mod`.
##' @param ... Additional arguments to pass to NMwriteSection.
##' @return Lines for a new control stream (invisibly)
##' @family Nonmem
##' 
##' @export

NMreplaceDataFile <- function(files,file.pattern,dir,path.data,newfile=file.mod,...){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    file.mod <- NULL
    quiet <- NULL
    

### Section end: Dummy variables, only not to get NOTE's in pacakge checks 


    
    if(missing(files)) files <- NULL
    if(missing(dir)) dir <- NULL
    if(missing(file.pattern)) file.pattern <- NULL

    all.files <- getFilePaths(files=files,file.pattern=file.pattern,dir=dir,quiet=quiet)
    
    ## newfile
    ## if(!missing(newfile)&&!is.null(newfile) && length(all.files)>1) {
    ##     stop("if multiple files are edited, newfile must be missing or NULL.")
    ## }

    is.missing.newfile <- missing(newfile)
    
    res <- lapply(all.files,function(file.mod){
        if(is.missing.newfile) newfile <- file.mod
        extr.data <- NMextractDataFile(file.mod)
        sec.data.new <- paste("$DATA",paste(sub(extr.data$string,path.data,extr.data$DATA,fixed=TRUE),collapse="\n"))
        NMwriteSection(files=file.mod,section="DATA",newlines=sec.data.new,newfile=newfile,...)
    })
    

    invisible(res)
}
