##' Merge columns onto all elements in a NM data object
##'
##' @description This is useful for adding columns to a nonmem data object as
##'     created by NMscanData because it does so to all elements containing the
##'     by column in the object. The merge call is run by mergeCheck with all.x=TRUE.
##' @param data A NM data object
##' @param df a data.frame to merge onto elements of data object.
##' @param by passed to merge
##' @param debug Start by calling browser()?
##' @family DataWrangling
##' 
##' @examples
##' df.races <- data.frame(RACE=c(1,3.1),
##'                        race1=c("White","Japanese"),
##'                        stringsAsFactors=FALSE)
##' @export

NMaddColumns <- function(data,df,by,debug=FALSE){
    if(debug) browser()
    data.new <- lapply(data,function(d){
        if(!is.data.frame(d)||!all(by%in%names(d))) return(d)
        d2 <- try(mergeCheck(d,df,by=by,all.x=T,debug=F))
        if("try-error"%in%class(d2)) d2 <- d
        d2
    })

}
