##' Read information from Nonmem ext files
##'
##' @param file Path to the ext file
##' @param return The .ext file contains both final parameter
##'     estimates and iterations of the estimates. If
##'     \code{return="pars"} (default) the final estimates are
##'     returned in addition to what other parameter-level information
##'     is found, like FIX, sd etc. as columns. If
##'     \code{return="iterations"}, the iterations are returned
##'     (including objective function value). If \code{return="obj"}
##'     objective function value at final estimate is returned. If
##'     \code{return="all"}, all er returned, though in separate
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

    . <- NULL
    blocksize <- NULL
    cond <- NULL
    eigCor <- NULL
    est <- NULL
    FIX <- NULL
    i <- NULL
    iblock <- NULL
    imin <- NULL
    j <- NULL
    ITERATION <- NULL
    NMREP <- NULL
    model <- NULL
    par.type <- NULL
    parameter <- NULL
    TABLENO <- NULL
    table.step <- NULL
    partLik <- NULL
    se <- NULL
    seStdDevCor <- NULL
    stdDevCor <- NULL
    value <- NULL
    variable <- NULL    
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(col.model!="model") stop("NMreadExt() currently only supports col.model=\"model\".")
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)
    if(missing(auto.ext) || is.null(auto.ext)) auto.ext <- TRUE

    if(is.null(tableno)) tableno <- "max"

    if( (is.character(tableno)&& !tableno%in%c("min","max","all") ) ||
        (is.numeric(tableno) && (tableno<=0 || tableno%%1!=0) )){
        stop("tableno must be either one of the character strings min, max, all or an integer greater than zero.")
    }
    
    ## args <- getArgs()
    args <- getArgs(sys.call(),parent.frame())
    if(missing(file.ext)) file.ext <- NULL
    file <- deprecatedArg("file.ext","file",args=args)

    if(missing(return)||is.null(return)) return <- "pars"
    return <- tolower(return)
    
    allowed.return <- c("pars","iterations","obj","all")
    if(!return %in% allowed.return){
        stop("Argument return has to be one of: ", paste(allowed.return,collapse =", "))
    }

    fun.file.ext <- NMdataDecideOption("file.ext")
    if(auto.ext){
        file <- fun.file.ext(file)
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
    -1e+09,value
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
    obj <- NULL
    if(nrow(pars)){
        
        pars <- melt(pars,id.vars=c(col.model,cc(TABLENO,NMREP,table.step,ITERATION,variable)),variable.name="parameter")
        pars <- dcast(pars,model+TABLENO+NMREP+table.step+parameter~variable,value.var="value")

        pars <- addParType(pars)

        setcolorder(pars,intersect(c(col.model,"TABLENO","NMREP","table.step","par.type","parameter","par.name","i","j","FIX","value", "cond","eigCor",   "partLik",   "se", "seStdDevCor", "stdDevCor", "termStat"),colnames(pars)))
        
        ## obj <- pars[parameter%in%c("SAEMOBJ","OBJ"),  .(model, TABLENO, NMREP, table.step, par.type,parameter,value)]
        obj <- pars[parameter%in%c("SAEMOBJ","OBJ")]
        cols.drop <- intersect(colnames(pars),cc(i,j,FIX,est,cond,eigCor ,partLik ,se ,seStdDevCor, stdDevCor ))
        obj[,(cols.drop):=NULL]
        pars <- pars[!parameter%in%c("SAEMOBJ","OBJ")]
        
### this setorder call doesnt work - unsure why
        ## setorder(pars,match(par.type,c("THETA","OMEGA","SIGMA")),i,j)
        pars <- pars[order(model,match(par.type,c("THETA","OMEGA","SIGMA")),i,j)]
        ## est is just a copy of value for backward compatibility
        pars[,est:=value]

### add OMEGA block information based on off diagonal values
        tab.blocks <- rbind(pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=i,j=j,value)],
                            pars[par.type%in%c("OMEGA","SIGMA"),.(par.type,i=j,j=i,value)])[
            abs(value)>1e-9,.(iblock=min(i,j),blocksize=max(abs(j-i))+1),by=.(par.type,i)]

        ## pars0 <- copy(pars)
        ## tab.blocks
        pars <- mergeCheck(pars,tab.blocks,by=cc(par.type,i),all.x=T,quiet=TRUE)

        ## pars[par.type%in%c("OMEGA","SIGMA"),.(i,j,iblock,blocksize,value)]

        ### this is insufficient. if 1-2 and 3-4 are blocks, (3,2) must not be part of any block.
        pars[abs(i-j)>(blocksize-1),(c("iblock","blocksize")):=list(NA,NA)]
        pars[!is.na(iblock),imin:=min(i),by=.(iblock)]
        pars[j<imin,(c("iblock","blocksize")):=list(NA,NA)]
        pars[,imin:=NULL]

        ## pars[par.type%in%c("OMEGA","SIGMA"),.(i,j,iblock,blocksize,imin,value)]
        
        pars[par.type%in%c("OMEGA","SIGMA")&i==j&is.na(iblock),iblock:=i]
        pars[par.type%in%c("OMEGA","SIGMA")&i==j&iblock==i&is.na(blocksize),blocksize:=1]
### done add OMEGA/SIGMA blocks
        
    }
    
    ## what to do about OBJ? Disregard? And keep in a iteration table instead?
    iterations <- res.NMdat[as.numeric(ITERATION)>(-1e9),!("variable")] 
    iterations <- addTableStep(iterations,keep.table.name=FALSE)
    iterations <- melt(iterations,id.vars=cc(model,TABLENO,NMREP,table.step,ITERATION),variable.name="parameter")
    iterations <- addParType(iterations)

    res <- list(pars=pars,iterations=iterations,obj=obj)
    res <- lapply(res,as.fun)

    if(return=="pars") return(res$pars)
    if(return=="iterations") return(res$iterations)
    if(return=="obj") return(res$obj)

    
    ## as.fun already applied
    res
}
