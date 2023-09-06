##' Compare elements in lists with aim of combining
##'
##' Useful interactive tool when merging or binding objects
##' together. It lists the names of elements that differ in presence
##' or class across multiple datasets. Before running rbind, you may want
##' to check the compatibility of the data.
##' 
##' @param ... objects which element names to compare
##' @param list.data As alternative to ..., you can supply the data
##'     sets in a list here.
##' @param keep.names If TRUE, the original dataset names are used in
##'     reported table. If not, generic x1, x2,... are used. The
##'     latter may be preferred for readability.
##' @param test.equal Do you just want a TRUE/FALSE to whether the
##'     names of the two objects are the same? Default is FALSE which
##'     means to return an overview for interactive use. You might
##'     want to use TRUE in programming. However, notice that this
##'     check may be overly rigorous. Many classes are compatible
##'     enough (say numeric and integer), and compareCols doesn't take
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
##' @param cols.wanted Columns of special interest. These will always
##'     be included in overview and indicated by a prepended * to the
##'     column names. This argument is often useful when you start by
##'     defining a set of columns that you want to end up with by
##'     combining a number of data sets.
##' @param quiet The default is to give some information along the way
##'     on what data is found. But consider setting this to TRUE for
##'     non-interactive use. Default can be configured using
##'     NMdataConf.
##' @param as.fun A function that will be run on the result before
##'     returning. If first input data set is a data.table, the
##'     default is to return a data.table, if not the default is to
##'     return a data.frame. Use whatever to get what fits in with
##'     your workflow. Default can be configured with NMdataConf.
##' @details technically, this function compares classes of elements in
##'     lists. However, in relation to NMdata, this will most of the
##'     time be columns in data.frames.
##' @param keepNames Deprecated. Use keep.names instead.
##' @param testEqual Deprecated. Use test.equal instead.
##' @return A data.frame with an overview of elements and their
##'     classes of objects in ... Class as defined by as.fun.
##' @details
##' Despite the name of the argument fun.class, it can be any
##' function to be evaluated on each element in `...`. See examples for how
##' to extract SAS labels on an object read with `read_sas` from
##' the `haven` package.
##' @examples
##' ## get SAS labels from objects read with haven::read_sas
##' \dontrun{
##' compareCols(...,fun.class=function(x)attributes(x)$label)
##' }
##' 
##'
##' @family DataWrangling
##' @export


compareCols <- function(...,list.data,keep.names=TRUE,test.equal=FALSE,diff.only=TRUE,cols.wanted,fun.class=base::class,quiet,as.fun,keepNames,testEqual){
    

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    value <- NULL
    column <- NULL
    nu <- NULL
    wanted <- NULL
    . <- function() NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)


    
    args <- getArgs()

    
    ## deprecated since 2023-06-10
    ## keep.names <- deprecatedArg("keepNames","keep.names",args=args)
    keep.names <- deprecatedArg("keepNames","keep.names",args=args)
    ## deprecated since 2023-06-10
    test.equal <- deprecatedArg("testEqual","test.equal",args=args)
    
    
    if(missing(list.data)){
        dots <- list(...)
        if(keep.names){
            names.dots <- setdiff(as.character(match.call(expand.dots=TRUE)),as.character(match.call(expand.dots=FALSE)))
        }
    } else {
        dots <- list.data
        names.dots <- names(dots)
        if(is.null(names.dots)) names.dots <- paste0("x",1:length(list.data))
        if(""%in%names.dots){
            names.dots[names.dots==""] <- paste0("x",which(names.dots==""))
        }
        names(dots) <- names.dots
    }

    

    ndots <- length(dots)
    if(ndots==0) stop("No data supplied.")
    if(ndots==1&&missing(diff.only)) diff.only <- FALSE
    if(missing(cols.wanted)) cols.wanted <- NULL


    if(!keep.names){
        names.dots <- paste0("x",seq(ndots))
    }
    names(dots) <- names.dots
    
    
    df1.was.dt <- is.data.table(dots[[1]])
    if(missing(as.fun)) as.fun <- NULL
    if(df1.was.dt && is.null(as.fun)) as.fun <- "data.table"
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    getClasses <- function(x){
        cls <- lapply(x,function(x)paste(fun.class(x),collapse=", "))
        data.table(column=names(cls),class=as.character(unlist(cls)))
    }
    
    cols <- lapply(dots,getClasses)
    for(n in 1:ndots) setnames(cols[[n]],"class",names.dots[n])

    col.wanted <- tmpcol(names=sapply(cols,names),base="wanted",prefer.plain=TRUE)
    if(!is.null(cols.wanted)) {
        dt.wanted <- data.table(column=cols.wanted)
        dt.wanted[,(col.wanted):=.I]
        ##    cols <- append(list(wanted=dt.wanted),cols)
    }
    
    dt.cols <- Reduce(function(...)merge(...,by="column",all=TRUE),cols)

    dt.cols.l <- melt(dt.cols,id.vars="column")
    
    nu.classes <- dt.cols.l[!is.na(value),.(nu=uniqueN(value),n=.N),by=.(column)]

    ## merge back on
    dt.cols <- mergeCheck(dt.cols,nu.classes,by="column",quiet=TRUE)
    
    
    if(test.equal) return(dt.cols[n<ndots|nu>1,.N]==0)

    if(is.null(cols.wanted)){
        dt.cols[,(col.wanted):=0]
    } else {
        dt.cols <- merge(dt.cols,dt.wanted,all=T)
        dt.cols[!is.na(get(col.wanted)),column:=paste0("*",column)]
    }

    dt.cols.full <- copy(dt.cols)
    
    ## criteria whether to show if nu=1 (all equal class)
    if(diff.only) dt.cols <- dt.cols[n<ndots|nu>1|get(col.wanted)>0]
### this one orders by number of occurance, unique classses, column name

    if(ndots>1){
        dt.cols[,nu:=-nu]
        dt.cols[is.na(wanted),wanted:=1e5]
        setorderv(dt.cols,c(col.wanted,"n","nu","column"))
    }
    cols.rm <- c("nu","n","wanted")
    dt.cols[,(cols.rm):=NULL]
    
    
### what about a summary of dimensions of the supplied datasets? We do that in the separate "dims" function.
    
    if(!quiet) {
        if(all(sapply(dots,is.data.frame))){

            res.dims <- dims(list.data=dots)
            message("Dimensions:")
            print(as.fun(res.dims))
        }

        if(nrow(dt.cols)==0&&ndots==1){
            message("Only one data set supplied.\n")
        } else if(nrow(dt.cols)==0&&ndots>1){
            message("No differences.\n")
        } else {
            if(diff.only){
                message("\nColumns that differ:")
                print(as.fun(dt.cols))
                message()
                messageWrap(paste0("\nColumns where no differences were found: ",paste(dt.cols.full[nu==1&n==ndots,column],collapse=", "),"."),fun.msg=message)
            } else {
                message("\nOverview of all columns:")
                print(as.fun(dt.cols))
            }
        }
        return(invisible(as.fun(dt.cols)))
    } else {
        return(as.fun(dt.cols))
    }
    

}


