##' Tidy paths
##'
##' @description Used to tidy up paths. Passes arguments to
##'     file.path(). The function is intended to return a canonical
##'     path format, i.e. paths that can be compared by simple string
##'     comparisson.
##' @param ... additional arguments passed to file.path().
##' @param debug start by running browser()?
##' @family FileSystem
##' @export

 

filePathSimple <- function(...,debug=F){
    if(debug) browser()
    
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
    fpath <- sub("^([a-z]):/","\\U\\1:/",fpath,perl=T)
    is.win.abs <- grepl("^[a-z]:/",fpath,ignore.case=T,perl=T)
    if(any(is.win.abs)){
        fpath[is.win.abs] <- normalizePath(fpath[is.win.abs],mustWork=FALSE,winslash="/")
    }
    return(fpath)
}
