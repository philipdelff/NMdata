##' add Omega correlations to a parameter table

##' @param pars A parameter table, like returned by `NMreadExt()`.
##' @param by The name of a column, as a string. Calculate the
##'     correlations within a grouping variable?  This will often be a
##'     column containing the model name.
##' @keywords internal
## Can be exported but needs as.fun and return
addOmegaCorr <- function(pars,by=NULL){
    pars <- as.data.table(pars)
    if(is.null(by)){
        pars.list <- list(pars)
    } else {
        pars.list <- split(pars,by=by)
    }
    
    res.list <- lapply(
        pars.list,
        function(x){
            Sigma <- NMsim:::dt2mat(x[par.type=="OMEGA"])
            mat.cor <- cov2cor(Sigma)
            dt.cor <- mat2dt(mat.cor)
            x <- mergeCheck(x,dt.cor[,.(par.type="OMEGA",i,j,corr=value)],by=cc(par.type,i,j),all.x=TRUE)
            x
        })
    
    rbindlist(res.list)
}

