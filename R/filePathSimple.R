##' Clean and standardize file system paths
##'
##' @description Use this to tidy up paths. Combines pieces of a path like
##'     file.path(). The function is intended to return a canonical
##'     path format, i.e. paths that can be compared by simple string
##'     comparison. Redundant /'s removed. normalizePath is used to
##'     possibly shorten path.
##' @param ... additional arguments passed to file.path().
##' @return A (character) file path
##' @family FileSystem
##' @keywords internal

filePathSimple <- function(...){
    
    fpath <- file.path(...)
    ## get rid of heading and tailing white spaces
    fpath <- trimws(fpath)
    ## convert double \\ into /
    fpath <- gsub(pattern="\\\\",replacement="/",x=fpath)
    ## removing redundant /'s that make it harder to compare paths. Not in beginning of paths, because they are network paths.
    slashes.lead <- sub("^(/*).*","\\1",fpath)
    after.slashes <- sub("^/+","",fpath)
    after.slashes2 <-  gsub(pattern="//",replacement="/",after.slashes)
    fpath <- paste0(slashes.lead,after.slashes2)
##    fpath <- gsub(pattern="(?<!^)/+",replacement="/",fpath)
    ## a dir path should not end in a / (which again, makes comparissons more complicate)
    fpath <- gsub(pattern="/+$",replacement="",x=fpath)
    
    
    ## Denote windows drives with capital letter
    fpath <- sub("^([a-z]):/","\\U\\1:/",fpath,perl=TRUE)
    is.win.abs <- grepl("^[a-z]:/",fpath,ignore.case=TRUE,perl=TRUE)
    if(any(is.win.abs)){
        fpath[is.win.abs] <- normalizePath(fpath[is.win.abs],mustWork=FALSE,winslash="/")
    }
    return(fpath)
}
