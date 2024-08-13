##' Drop leading, trailing and repeated spaces in character strings
##' @param x A vector of character strings to modify
##' @param double Replace any number of consecutive blank spaces by a
##'     single blank. Default is TRUE.
##' @param lead Drop spaces before first non-empty character. Default
##'     is TRUE.
##' @param trail Drop spaces after last non-empty character. Default
##'     is TRUE.
##' @keywords internal
##' 
cleanSpaces <- function(x,double=TRUE,lead=TRUE,trail=TRUE){
    if(double) x <- gsub(paste0(" +")," ",x)
    if(lead) x <- sub(paste0("^ +"),"",x)
    if(trail) x <- sub(paste0(" +$"),"",x)
    x
}
