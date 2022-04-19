##' Change file name extension
##' 
##' Very simple but often applicable function to change the file name extension (from say file.lst to file.mod)
##' @param fn file name
##' @param ext new file name extension
##' @return A text string
##' @examples
##' fnExtension("file.lst",".mod")
##' fnExtension("file.lst","")
##' @export

fnExtension <- function(fn,ext){
    ## fn.new <- sub("\\.[^\\.]+$",ext,fn)
    fn.new <- sub("\\.[^\\.]+$","",fn)
    fn.new <- paste0(fn.new,ext)
    fn.new
}
