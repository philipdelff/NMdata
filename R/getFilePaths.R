##' Internal interpretation of file specification options
##' @param files character vector of full file paths. Specify either
##'     files or both file.pattern and dir.
##' @param file.pattern A regular expression to look for in dir. If
##'     used, dir must also be supplied.
##' @param dir The directory i which to look for file.pattern. dir is
##'     passed to list.files as pattern. If supplied, file.pattern
##'     must also be supplied.
##' @param quiet The default is not to be quiet.
##' @return A character vector of full paths to files
##' @keywords internal

getFilePaths <- function(files=NULL,file.pattern=NULL,dir=NULL,quiet){
    
    if(missing(quiet)) quiet <- NULL
    if(is.null(quiet)) quiet <- FALSE
    
    ## supply either file or file.pattern. dir only allowed if file.pattern
    if( is.null(files) && is.null(file.pattern) ){
        stop("You have to supply either file or file.pattern")
    }
    if(!is.null(files)&& (!is.null(file.pattern) || !is.null(dir))){
        stop("If supplying files, file.pattern and dir cannot be used")
    }
    if(!is.null(file.pattern)&&is.null(dir)){
        stop("If using file.pattern, you have to supply dir too.")
    }
    
    if(!is.null(files)&&length(files)>0){
        
        if(any(!file.exists(files))){
            if(!quiet){
                message("Files not found. Skipping:\n",paste(files[!file.exists(files)],collapse="\n"))
            }
        }
        all.files <- files[file.exists(files)]
    }
    
    
    if(!is.null(file.pattern)){
        all.files <- list.files(path=dir,pattern=file.pattern,full.names=TRUE,recursive=FALSE)
    }
    
    if(length(all.files)==0){
        message("No existing files matched. Nothing to do.")
        return(invisible(NULL))
    }

    return(all.files)
}

