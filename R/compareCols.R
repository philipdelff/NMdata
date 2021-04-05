##' Compare elements in lists with aim of combining
##'
##' Useful interactive tool when merging or binding objects
##' together. It lists the names of elements that differ in presence
##' or class across multiple datasets. Before running rbind, you may want
##' to check the compatibility of the data.
##' 
##' @param ... objects which element names to compare
##' @param keepNames If TRUE, the original dataset names are used in
##'     reported table. If not, generic x1, x2,... are used. The
##'     latter may be preferred for readability.
##' @param testEqual Do you just want a TRUE/FALSE to whether the
##'     names of the two objects are the same? Default is FALSE which
##'     means to return an overview for interactive use. You might
##'     want to use TRUE in programming. However, notice that this
##'     check may be overly rigorous. Many classes are compitable
##'     enough (say numeric and integer), at compareCols doesn't take
##'     this into account.
##' @param diff.only Don't report columns where no difference
##'     found.
##' @param fun.class the function that will be run on each column to
##'     check for differences. base::class is default. Notice that the
##'     alternative base::typeof is different in certain ways. For
##'     instance, typeof will not report a difference on numeric vs
##'     difftime. You could basically submit any function that takes a
##'     vector and returns a single value.
##' @family DataWrangling
##' @export



## tecnically, this function compares classes of elements in
## lists. However, in relation NMdata, this will most of the time be
## columns in data.frames.



compareCols <- function(...,keepNames=T,testEqual=F,diff.only=TRUE,fun.class=base::class){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    value <- NULL
  element <- NULL
nu <- NULL
    . <- function() NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    
    dots <- list(...)
    ndots <- length(dots) 
    if(ndots<2) stop("At least two objects must be supplied")
    if(keepNames){
        names.dots <- setdiff(as.character(match.call(expand.dots=T)),as.character(match.call(expand.dots=F)))
    } else {
        names.dots <- paste0("x",seq(ndots))
    }

    getClasses <- function(x){
        cls <- lapply(x,fun.class)
        data.table(element=names(cls),class=as.character(unlist(cls)))
    }
    cols <- lapply(dots,getClasses)
    for(n in 1:ndots) setnames(cols[[n]],"class",names.dots[n])

    
    dt.cols <- Reduce(function(...)merge(...,by="element",all=T),cols)

    dt.cols.l <- melt(dt.cols,id.vars="element")
    nu.classes <- dt.cols.l[,.(nu=uniqueN(value),n=.N),by=.(element)]

    ## merge back on
    dt.cols <- mergeCheck(dt.cols,nu.classes,by="element")

    if(testEqual) return(dt.cols[n<ndots|nu>1,.N]==0)

    ## criteria whether to show if nu=1 (all equal class)
    ## sorting options. By diff, alpha. Or by diff, location in first df?

    if(diff.only) dt.cols <- dt.cols[n<ndots|nu>1]
    ### this one orders by number of occurance, unique classses, element name
    dt.cols <- dt.cols[order(-n,-nu,element)][,!c("nu","n")]

### what about a summary of dimensions of the supplied datasets? We do that in the separate "dims" function.
    
    return(dt.cols)

}


## compareCols(r1.csv,r1.rds)
## compareCols(r1.csv,r1.rds,fun.class=typeof)