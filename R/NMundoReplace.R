##' Undo NMreplacePart
##' simply overwrite input.txt with contents of backup_input.txt
##' @param file The file to recover
##' @param debug start by running browser()?
##' @family Nonmem
##' @export

NMundoReplace <- function(file,debug=F){
    if(debug) browser()
    
    file <- filePathSimple(file)
## the file to recover
    file.backup <- file.path(dirname(file),paste0("backup_",basename(file)))
## not to lose anything, a backup of the input file to be overwritten is kept in trash.
    file.trash <- file.path(dirname(file),paste0("trashed_",basename(file)))
    
    stopifnot(file.exists(file))
    stopifnot(file.exists(file.backup))
    
    file.copy(file,file.trash,overwrite=T)
    file.copy(file.backup,file,overwrite=T)


}

