##' Read information from Nonmem ext files
##'
##' @param file.ext Path to the ext file
##' @param return The .ext file contains both final parameter
##'     estimates and iterations of the estimates. If
##'     \code{return="pars"} (default) the final estimates are
##'     returned in addition to what other parameter-level information
##'     is found. If \code{return="iterations"}, the iterations are
##'     returned. If \code{return="both"}, both er returned, though in
##'     separate data.frames compiled in a list.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @param modelname See ?NMscanData
##' @param col.model See ?NMscanData
##' @return A list with a final parameter table and a table of the
##'     iterations
##' @import data.table
##' @export

NMreadExt <- function(file.ext,return="pars",as.fun,modelname,col.model){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    ITERATION <- NULL
    variable <- NULL
    NMREP <- NULL
    model <- NULL
    par.type <- NULL
    parameter <- NULL
    i <- NULL
    j <- NULL
    TABLENO <- NULL
    table.step <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)

    allowed.return <- c("pars","iterations","all")
    if(!return %in% allowed.return){
        stop("Argument return has to be one of: ", paste(allowed.return,collapse =", "))
    }

    

    res.NMdat <- lapply(file.ext,function(file){
        this.model <- modelname(file)
        NMreadTab(file,as.fun="data.table",quiet=TRUE,col.table.name=TRUE)[,(col.model):=this.model]
    })
    res.NMdat <- rbindlist(res.NMdat,fill=TRUE)

    ## NONMEM USERS GUIDE
    ## INTRODUCTION TO NONMEM 7.5.0
    ## Robert J. Bauer
    ## ICON Plc
    ## Gaithersburg, Maryland
    ## February 23, 2021

    dt.codes <- fread(text="ITERATION,variable
    -1e+09,est
    -1000000001,se
    -1000000002,eigCor
    -1000000003,cond
    -1000000004,stdDevCor
    -1000000005,seStdDevCor
    -1000000006,FIX
    -1000000007,termStat
    -1000000008,partLik")

    ## dt.codes

    res.NMdat <- mergeCheck(res.NMdat,dt.codes,by=cc(ITERATION),all.x=T,quiet=TRUE)
    ## res.NMdat

    
    
    pars <- res.NMdat[variable%in%dt.codes$variable,setdiff(colnames(res.NMdat),"OBJ"),with=FALSE]
    
    pars <- addTableStep(pars,keep.table.name=FALSE)
    
    pars <- melt(pars,id.vars=cc(model,TABLENO,NMREP,table.step,ITERATION,variable),variable.name="parameter")
    pars <- dcast(pars,model+TABLENO+NMREP+table.step+parameter~variable,value.var="value")

    pars[,par.type:=NA_character_]
    pars[grepl("^THETA",parameter),par.type:="THETA"]
    pars[grepl("^OMEGA",parameter),par.type:="OMEGA"]
    pars[grepl("^SIGMA",parameter),par.type:="SIGMA"]
    pars[par.type=="THETA",i:=sub("THETA([0-9]+)","\\1",parameter)]
    pars[par.type=="OMEGA",i:=sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\1",parameter)]
    pars[par.type=="OMEGA",j:=sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\2",parameter)]
    pars[par.type=="SIGMA",i:=sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\1",parameter)]
    pars[par.type=="SIGMA",j:=sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\2",parameter)]
    cols <- cc(i,j)
    pars[,(cols):=lapply(.SD,as.integer),.SDcols=cols]

    ## what to do about OBJ? Disregard? And keep in a iteration table instead?
    iterations <- res.NMdat[as.numeric(ITERATION)>(-1e9),!("variable")] 
    iterations <- addTableStep(iterations,keep.table.name=FALSE)
    iterations <- melt(iterations,id.vars=cc(model,TABLENO,NMREP,table.step,ITERATION),variable.name="parameter")

    res <- list(pars=pars,iterations=iterations)
    res <- lapply(res,as.fun)

    if(return=="pars") return(res$pars)
    if(return=="iterations") return(res$iterations)

    as.fun(res)
}
