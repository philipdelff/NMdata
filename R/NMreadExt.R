##' Read information from Nonmem ext files
##'
##' @param file Path to the ext file
##' @param return The .ext file contains both final parameter
##'     estimates and iterations of the estimates. If
##'     \code{return="pars"} (default) the final estimates are
##'     returned in addition to what other parameter-level information
##'     is found, like FIX, sd etc. as columns. If
##'     \code{return="iterations"}, the iterations are returned. If
##'     \code{return="both"}, both er returned, though in separate
##'     data.frames compiled in a list.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say `tibble::as_tibble`) in as.fun to convert to
##'     something else. If `data.table`s are wanted, use
##'     `as.fun="data.table"`. The default can be configured using
##'     `NMdataConf()`.
##' @param tableno In case the ext file contains multiple tables, this
##'     argument controls which one to choose. The options are
##'     \itemize{
##' 
##' \item "max" (default) Pick the table with the highest table
##' number. This typically means the results from the last
##' `$ESTIMATION` step are used.
##' 
##' \item "min" Pick results from the first table available.
##'
##' \item "all" Keep all results. The tables can be distinguished by
##' the `tableno` column.
##'
##' \item an integer greater than 0, in which case the table with this
##' table number will be picked.
##' }
##' @param modelname See `?NMscanData`
##' @param col.model See `?NMscanData`
##' @param auto.ext If `TRUE` (default) the extension will automatically
##'     be modified using `NMdataConf()$file.ext`. This means `file`
##'     can be the path to an input or output control stream, and
##'     `NMreadExt` will still read the `.ext` file.
##' @param file.ext Deprecated. Please use \code{file} instead.
##' @details The parameter table returned if \code{return="pars"} or \code{return="all"} will contain columns based on the Nonmem 7.5 manual. It defines codes for different parameter-level values. They are:
##'
##' -1e+09: se
##' -1000000002: eigCor
##' -1000000003: cond
##' -1000000004: stdDevCor
##' -1000000005: seStdDevCor
##' -1000000006: FIX
##' -1000000007: termStat
##' -1000000008: partLik
##'
##' The parameter name is in the \code{parameter} column. The
##' "parameter type", like "THETA", "OMEGA", "SIGMA" are available in
##' the \code{par.type} column. Counters are available in \code{i} and
##' \code{j} columns. \code{j} will be \code{NA} for
##' \code{par.type=="THETA"}
##'
##' The objective function value is included as a parameter.
##'
##' Notice that in case multiple tables are available in the `ext`
##' file, the column names are taken from the first table. E.g., in
##' case of SAEM/IMP estimation, the objective function values will be
##' in the `SAEMOBJ` column, even for the IMP step. This may change in
##' the future.
##' 
##' @return If \code{return="all"}, a list with a final parameter
##'     table and a table of the iterations. If \code{return="pars"},
##'     only the parameter table, and if \code{return="iterations"}
##'     only the iterations table. If you need both, it may be more
##'     efficient to only read the file once and use
##'     \code{return="all"}. Often, only one of the two are needed,
##'     and it more convenient to just extract one.
##' @import data.table
##' @export

NMreadExt <- function(file,return,as.fun,modelname,col.model,auto.ext,tableno="max",file.ext){
    
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
    if(missing(auto.ext) || is.null(auto.ext)) auto.ext <- TRUE

    if(is.null(tableno)) tableno <- "max"

    if( (is.character(tableno)&& !tableno%in%c("min","max","all") ) ||
        (is.numeric(tableno) && (tableno<=0 || tableno%%1!=0) )){
        stop("tableno must be either one of the character strings min, max, all or an integer greater than zero.")
    }
    
    args <- getArgs()
    if(missing(file.ext)) file.ext <- NULL
    file <- deprecatedArg("file.ext","file",args=args)

    if(missing(return)||is.null(return)) return <- "pars"
    allowed.return <- c("pars","iterations","all")
    if(!return %in% allowed.return){
        stop("Argument return has to be one of: ", paste(allowed.return,collapse =", "))
    }

    fun.file.ext <- NMdataDecideOption("file.ext")
    if(auto.ext){
        file <- fun.file.ext(file)
    }


    addPartype <- function(pars){
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
        pars[]
    }

    res.NMdat <- lapply(file,function(file){
        this.model <- modelname(file)
        NMreadTab(file,as.fun="data.table",quiet=TRUE,col.table.name=TRUE)[,(col.model):=this.model]
    })

    if(tableno=="min"){
        res.NMdat <- lapply(res.NMdat,function(x)x[TABLENO==min(TABLENO)])
    }
    if(tableno=="max"){
        res.NMdat <- lapply(res.NMdat,function(x)x[TABLENO==max(TABLENO)])
    }
    if(is.numeric(tableno)){
        res.NMdat <- lapply(res.NMdat,function(x)x[TABLENO==tableno])
    }
    


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

    ## pars <- res.NMdat[variable%in%dt.codes$variable,setdiff(colnames(res.NMdat),"OBJ"),with=FALSE]
    pars <- res.NMdat[variable%in%dt.codes$variable]
    pars <- addTableStep(pars,keep.table.name=FALSE)
    if(nrow(pars)){
        pars <- melt(pars,id.vars=cc(model,TABLENO,NMREP,table.step,ITERATION,variable),variable.name="parameter")
        pars <- dcast(pars,model+TABLENO+NMREP+table.step+parameter~variable,value.var="value")

        pars <- addPartype(pars)
    }
    
    ## what to do about OBJ? Disregard? And keep in a iteration table instead?
    iterations <- res.NMdat[as.numeric(ITERATION)>(-1e9),!("variable")] 
    iterations <- addTableStep(iterations,keep.table.name=FALSE)
    iterations <- melt(iterations,id.vars=cc(model,TABLENO,NMREP,table.step,ITERATION),variable.name="parameter")
    iterations <- addPartype(iterations)

    res <- list(pars=pars,iterations=iterations)
    res <- lapply(res,as.fun)

    if(return=="pars") return(res$pars)
    if(return=="iterations") return(res$iterations)

    as.fun(res)
}
