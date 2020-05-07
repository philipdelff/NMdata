##' combine NMdata objects
##' import data.table
##'
##' @param ... The NMdata objects to combine
##' @details Each element in a NMdata object is a data.frame or
##'     data.table. The name of the element represents the variability
##'     level represented in the data.frame. Hence, this function
##'     rbinds elements at equal varibility level, say obj1$row and
##'     obj2$row.
##' @return If the first table in the first argument is a data.table,
##'     unkeyed data.tables will be returned. If not, data.frames will
##'     be returned.
##' @export

rbind.NMdata <- function(...){

    
    dots <- list(...)
    as.dt <- is.data.table(dots[[1]][[1]])
    ## names.dots <- setdiff(
    ##     as.character(match.call(expand.dots=T))
    ##    ,
    ##     as.character(match.call(expand.dots=F))
    ## )
    names.dots <- paste0("x",seq(length(dots)))

    
    names.elements <- lapply(dots,function(x){
        elem.is.null <- sapply(x,is.null)
        sort(names(x[!elem.is.null]))
        })
    allnms <- unique(do.call(c,names.elements))
    
    mat.nms <- do.call(data.frame,lapply(names.elements,function(x)ifelse(allnms%in%x,rep(1,length(allnms)),rep(0,length(allnms)))))

    rownames(mat.nms) <- allnms
    colnames(mat.nms) <- names.dots

    
    newlist <- list()
    for(tab in allnms){
        newlist[[tab]] <- do.call(rbind,c(lapply(dots,function(x)as.data.table(x[[tab]])),list(fill=T)))
    }
    class(newlist) <- "NMdata"
    if(as.dt) newlist <- lapply(newlist,as.data.frame)

    newlist

}
