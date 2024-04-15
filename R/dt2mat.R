##' Convert a data.table of parameter estimates to a matrix
##'
##' Often needed when using estimates of Omega or Sigma matrices in
##' further calculations.
##' @param pars A data.table with parameters. Must contain columns `i`
##'     and `j` with row and column indexes and `est` with parameter
##'     (matrix) values.
##' @param dt.subset Specifies whether pars contains only a lower or
##'     upper triangle of an assumed symmetric matrix (most often the
##'     case for variance-covariance matrices), or it contains the
##'     full matrix.
##' @param max.i By default, the maximum row number is derived as he
##'     maximum value in the `i` column. If more (empty ones) are
##'     needed, specify the maximum row number with `max.i`. This can
##'     be necessary in cases where only estimated elements are
##'     available but a full matrix including elements related to
##'     fixed parameters is needed.
##' @param fill Value to insert for missing elements
##' @details If pars does not contain all `i` values, they will be
##'     imputed with zeros. The desired matrix dimension is inferred
##'     from `min(i)` and `max(i)`.  In case `dt.subset=="unique"`
##'     missing `j` elements will also give imputations of missing
##'     elements.
##' @import data.table
##' @return a matrix
##' @export

dt2mat <- function(pars,dt.subset="unique",max.i,fill=0){

    . <- NULL
    est <- NULL
    i <- NULL
    j <- NULL

    if(!dt.subset%in%cc(unique,all)) {
        stop("`dt.subset` must be either `unique` or `all`.")
    }
    
    pars.mat <- pars[,.(i,j,est)]

    if(dt.subset=="unique"){
        pars.mat <- rbind(pars.mat,
                          pars[i!=j][,.(i=j,j=i,est)]
                         ,fill=T)
    }

    ## If not all i's are provided, set them to zero
    if(missing(max.i) || is.null(max.i)) max.i <- max(pars$i)
    i.missing <- setdiff(min(pars$i):max.i,pars$i)
    pars.mat <- rbind(pars.mat,data.table(i=i.missing,j=i.missing),fill=TRUE)

    ## note, dcast returns a keyed data.table (keys are LHS vars) so it is always ordered by i.
    matrix.pars <- as.matrix(dcast(pars.mat,i~j,value.var="est")[,!("i")])
    if(!isFALSE(fill)){
        matrix.pars[is.na(matrix.pars)] <- fill
    }
    matrix.pars
}
