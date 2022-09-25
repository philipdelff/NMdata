##' Change file name extension
##' 
##' Very simple but often applicable function to change the file name extension (from say file.lst to file.mod)
##' @param fn file name
##' @param ext new file name extension.
##' @return A text string
##' @examples
##' fnExtension("file.lst",".mod")
##' fnExtension("file.lst","mod")
##' fnExtension("file.lst","..mod")
##' fnExtension("file.lst",cc(.mod,xml))
##' fnExtension(cc(file1.lst,file2.lst),cc(.xml))
##' fnExtension(cc(file1.lst,file2.lst),cc(.xml,.cov))
##' fnExtension("file.lst","")
##' @export

fnExtension <- function(fn,ext){
    ext <- sub("^ *\\.{0,1}(.+)","\\.\\1",ext)
    fn.new <- sub("\\.[^\\.]+$","",fn)
    fn.new <- paste0(fn.new,ext)
    fn.new
}
