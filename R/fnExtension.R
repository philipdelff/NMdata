##' Change file name extension
##' 
##' Very simple but often applicable function to retrieve or change
##' the file name extension (from say file.lst to file.mod)
##' @param fn file name. Often ending in an extension after a period
##'     but the extension is not needed.
##' @param ext new file name extension. If omitted or NULL, the
##'     extension of fn is returned.
##' @return A text string
##' @examples
##' fnExtension("file.lst",".mod")
##' fnExtension("file.lst","mod")
##' fnExtension("file.lst","..mod")
##' fnExtension("file.lst",cc(.mod,xml))
##' fnExtension(cc(file1.lst,file2.lst),cc(.xml))
##' fnExtension(cc(file1.lst,file2.lst),cc(.xml,.cov))
##' fnExtension("file.lst","")
##' fnExtension("file.lst")
##' @export

fnExtension <- function(fn,ext){

    if(missing(ext) || is.null(ext)){
        ## disregarding any parent dirs        
        fn <- sub(".*/","",fn)
        ## if no dot in file name, there is no extension
        fn[!grepl("\\.",fn)] <- ""
        return(
            sub(".*\\.([^/.]*)","\\1",x=fn)
        )
    }

    ext <- sub("^ *\\.{0,1}([^/]+)","\\.\\1",ext)
    fn.new <- sub("\\.[^/\\.]+$","",fn)
    fn.new <- paste0(fn.new,ext)
    fn.new
}
