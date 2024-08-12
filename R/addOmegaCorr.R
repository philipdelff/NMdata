##' add Omega correlations to a parameter table

##' @param pars A parameter table, like returned by `NMreadExt()`.
##' @param by The name of a column, as a string. Calculate the
##'     correlations within a grouping variable?  This will often be a
##'     column containing the model name.
##' @param as.fun See `?NMdataConf`
##' @param col.value The name of the column from which to take the
##'     `OMEGA` values. Default is "value" in alignment with the
##'     output from `NMreadExt()`.
##' @return The parameter table with a `corr` column added.
##' @import data.table
##' @importFrom stats cov2cor
##' @export
##' 
## Can be exported but needs as.fun and return

addOmegaCorr <- function(pars,by=NULL,as.fun,col.value="value"){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    par.type <- NULL
    i <- NULL
    j <- NULL
    value <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks


    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    pars <- as.data.table(pars)
    if(is.null(by)){
        pars.list <- list(pars)
    } else {
        pars.list <- split(pars,by=by)
    }
    
    res.list <- lapply(
        pars.list,
        function(x){
            Sigma <- dt2mat(x[par.type=="OMEGA"],col.value=col.value)
            mat.cor <- suppressWarnings(cov2cor(Sigma))
            dt.cor <- mat2dt(mat.cor,triangle="all")
            
            x <- mergeCheck(x,dt.cor[,.(par.type="OMEGA",i,j,corr=get(col.value))],by=cc(par.type,i,j),all.x=TRUE)
            x
        })
    
    as.fun(rbindlist(res.list))
}

