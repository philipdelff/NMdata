##' @param internal
cleanSpaces <- function(x,double=TRUE,lead=TRUE,trail=TRUE){
    if(double) x <- gsub(paste0(" +")," ",x)
    if(lead) x <- sub(paste0("^ +"),"",x)
    if(trail) x <- sub(paste0(" +$"),"",x)
    x
}
