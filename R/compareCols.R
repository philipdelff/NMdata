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
##' @param diff.only If TRUE, don't report columns where no difference
##'     found. Default is TRUE if number of data sets supplied is
##'     greater than one. If only one data set is supplied, the full
##'     list of columns is shown by default.
##' @param fun.class the function that will be run on each column to
##'     check for differences. base::class is default. Notice that the
##'     alternative base::typeof is different in certain ways. For
##'     instance, typeof will not report a difference on numeric vs
##'     difftime. You could basically submit any function that takes a
##'     vector and returns a single value.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @param as.fun A function that will be run un the result before
##'     returning. If first input data set is a data.table, the
##'     default is to return a data.table, if not the default is to
##'     return a data.frame. Use whatever to get what fits in with
##'     your workflow. Default can be configured with NMdataConf.
##' @details tecnically, this function compares classes of elements in
##'     lists. However, in relation to NMdata, this will most of the
##'     time be columns in data.frames.
##' @return A data.frame with an overview of elementes and their
##'     classes of objects in ... Class as defined by as.fun.
##' @family DataWrangling
##' @export


compareCols <- function(...,keepNames=TRUE,testEqual=FALSE,diff.only=TRUE,fun.class=base::class,quiet,as.fun=NULL){
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    value <- NULL
    column <- NULL
    nu <- NULL
    . <- function() NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    dots <- list(...)
    ndots <- length(dots)
    if(ndots==0) stop("No data supplied.")
    if(ndots==1&&missing(diff.only)) diff.only <- FALSE
    
    if(keepNames){
        
        names.dots <- setdiff(as.character(match.call(expand.dots=TRUE)),as.character(match.call(expand.dots=FALSE)))
    } else {
        names.dots <- paste0("x",seq(ndots))
    }
    names(dots) <- names.dots
    
    df1.was.dt <- is.data.table(dots[[1]])
    if(df1.was.dt && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    getClasses <- function(x){
        cls <- lapply(x,fun.class)
        data.table(column=names(cls),class=as.character(unlist(cls)))
    }
    
    cols <- lapply(dots,getClasses)
    for(n in 1:ndots) setnames(cols[[n]],"class",names.dots[n])

    
    dt.cols <- Reduce(function(...)merge(...,by="column",all=TRUE),cols)

    dt.cols.l <- melt(dt.cols,id.vars="column")
    
    nu.classes <- dt.cols.l[!is.na(value),.(nu=uniqueN(value),n=.N),by=.(column)]

    ## merge back on
    dt.cols <- mergeCheck(dt.cols,nu.classes,by="column")

    if(testEqual) return(dt.cols[n<ndots|nu>1,.N]==0)

    ## criteria whether to show if nu=1 (all equal class)
    ## sorting options. By diff, alpha. Or by diff, location in first df?

    if(diff.only) dt.cols <- dt.cols[n<ndots|nu>1]
### this one orders by number of occurance, unique classses, column name

    if(ndots>1){
        setorder(dt.cols,n,-nu,column)
    }
    cols.rm <- c("nu","n")
    dt.cols[,(cols.rm):=NULL]
    
    
### what about a summary of dimensions of the supplied datasets? We do that in the separate "dims" function.
    
    if(!quiet) {
        message("Dimensions:")
        print(dims(list.data=dots))
    }
    message("\nOverview of columns:")
    if(nrow(dt.cols)==0&&ndots==1){
        message("Only one data set supplied.\n")
    } else if(nrow(dt.cols)==0&&ndots>1){
        message("No differences.\n")
    } else {
        print(dt.cols)
    }
    invisible(as.fun(dt.cols))

}


## compareCols(r1.csv,r1.rds)
## compareCols(r1.csv,r1.rds,fun.class=typeof)
