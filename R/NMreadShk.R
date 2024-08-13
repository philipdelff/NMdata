##' Read Shrinkage data reported by Nonmem
##'
##' @param file A model file. Extension will be replaced by ".shk".
##' @param auto.ext If `TRUE` (default) the extension will automatically
##'     be modified using `NMdataConf()$file.shk`. This means `file`
##'     can be the path to an input or output control stream, and
##'     `NMreadShk` will still read the `.shk` file.
##' @param as.fun See ?NMdataConf
##' @return A `data.frame` with shrinkage values, indexes, and name of related parameter, like `OMEGA(1,1)`.
##'
##' @details 
##' Type 1=etabar
##' Type 2=Etabar SE
##' Type 3=P val
##' Type 4=%Eta shrinkage SD version
##' Type 5=%EPS shrinkage SD version
##' Type 6=%Eta shrinkage based on empirical Bayes Variance (SD version)
##' Type 7=number of subjects used.
##' Type 8=%Eta shrinkage variance version
##' Type 9=%Eta shrinkage based on empirical Bayes Variance (variance version)
##' Type 10=%EPS shrinkage variance version
##' Type 11=%Relative information
##'
##' @import data.table
##' @export

NMreadShk <- function(file,auto.ext,as.fun){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    i <- NULL
    j <- NULL
    variable <- NULL
    par.type <- NULL
    parameter <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    if(missing(auto.ext) || is.null(auto.ext)) auto.ext <- TRUE
    fun.file.shk <- NMdataDecideOption("file.shk")
    if(auto.ext){
        file <- fun.file.shk(file)
    }
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    dt.type.shk <- fread(text="TYPE,shk.type,shk.name
1,etabar,etabar
2,Etabar SE,etabarSE
3,P val,Pval
4,%Eta shrinkage SD version,EtaShkSD
5,%EPS shrinkage SD version,EpsShkSD
6,%Eta shrinkage based on empirical Bayes Variance (SD version),EtaShkEbvSD
7,number of subjects used.,Nshk
8,%Eta shrinkage variance version,EtaShkVar
9,%Eta shrinkage based on empirical Bayes Variance (variance version),EtaShkEbvVar
10,%EPS shrinkage variance version,EpsShkVar
11,%Relative information,RelInfo")

    
    list.shk <- NMreadTabSlow(file)
    
    shk <- list.shk[[length(list.shk)]]

    cols.etas <- colnames(shk)[grepl("^ETA",colnames(shk))]
    dt.shk <- melt(shk,measure.vars=cols.etas)
    dt.shk <- mergeCheck(dt.shk,dt.type.shk,by="TYPE",quiet=TRUE)

    
    dt.shk[,i:=as.integer(sub("^ETA\\(([1-9][0-9]*)\\)$","\\1",variable))]
    dt.shk[,j:=i]
    ##dt.shk[,par.type:="OMEGA"]
    dt.shk[,parameter:=sprintf("OMEGA(%d,%d)",i,j)]
    dt.shk <- addParType(dt.shk)
    dt.shk.w <- dcast(dt.shk,variable+par.type+parameter+par.name+i+j~shk.name,value.var="value")

    dt.shk.w <- as.fun(dt.shk.w)
    
    return(dt.shk.w)
}


