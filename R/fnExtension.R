##' change file name extension
##' Very simple but often applicable function to change the file name extension (from say file.lst to file.mod)
##' @param fn file name
##' @param ext new file name extension
##' @export
##' @examples
##' fnExtension("file.lst",".mod")
##' fnExtension("file.lst","")

fnExtension <- function(fn,ext){
        fn.new <- sub("\\.[^\\.]+$",ext,fn)
        fn.new
}
