##' upper or lower triangle of a matrix as long-format
##' @param x A matrix
##' @param triangle Either `"lower"` (default) or `"upper"`, or all
##'     for which triangle to return. The two are equivalent for
##'     covariance or correlation matrices but the returned indexes
##'     will differ. "all" will return the full matrix which mostly
##'     makes sense if matrix is not a covariance or correlation
##'     matrix.
##' @param as.fun See `?NMdataConf`
##' @return A `data.frame`-like object with indexes `i` and `j` for
##'     position and matrix element value in `value` column.
##' @import data.table
##' @export

mat2dt <- function(x,triangle="lower",as.fun){
    

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    i <- NULL
    j <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    names.i <- rownames(x)
    if(is.null(names.i)) names.i <- as.character(1:nrow(x))
    names.j <- colnames(x)
    if(is.null(names.j)) names.j <- as.character(1:ncol(x))
    
    dt <- copy(as.data.table(x))
    
    colnames(dt) <- as.character(1:ncol(dt))
    dt[,i:=.I]
    
    dt <- melt(dt,id.vars="i",variable.name="j")
    dt[,j:=as.numeric(j)]

    dt <- mergeCheck(dt,
                     data.table(i=1:length(names.i),parameter.i=names.i) ,
                     by="i")
    dt <- mergeCheck(dt,data.table(j=1:length(names.j),parameter.j=names.j),by="j")

    dt <- switch(triangle,
                 upper=dt[j<=i]
                ,lower=dt[i<=j]
                ,all=dt
                 )

    as.fun(dt)
}
