##' Convert a data.table of parameter estimates to a matrix
##'
##' Often needed when using estimates of Omega or Sigma matrices in
##' further calculations.
##' @param pars A data.table with parameters like a subjset of the
##'     parameter list returned by NMreadExt. It MUST ONLY INCLUDE THE
##'     UNIQUE VALUES, so a upper or lower triangle of the matrix, not
##'     the full matrix.
##' @import data.table
##' @return a matrix
##' @export

dt2mat <- function(pars){

    . <- NULL
    est <- NULL
    i <- NULL
    j <- NULL

    pars.mat <- rbind(pars,
                      pars[i!=j]
                      [,.(i=j,j=i,est)]
                     ,fill=T)
    ## note, dcast returns a keyed data.table (keys are LHS vars) so it is always ordered by i.
    matrix.pars <- as.matrix(dcast(pars.mat,i~j,value.var="est")[,!("i")])

    matrix.pars
}
