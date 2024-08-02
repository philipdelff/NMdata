##' @details `NMrelate()` processes $PRED, $PK and $ERROR sections. It
##'     does not read ext files or $THETA, $OMEGA, $SIGMA sections to
##'     gain information but only extracts what it can from the model
##'     code. You can then merge with information from functions such
##'     as `NMreadExt()` and `NMreadParText()`.

NMrelate <- function(file,lines,pars,modelname,col.model,as.fun){
    

    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL

    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)
    

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)


    lines <- getLines(file=file,lines=lines,col.model=col.model,as.one=TRUE)


    types.pars <- cl("THETA","OMEGA","SIGMA")
    list.relate <- lapply(types.pars,function(tp) {
        ## NMrelateOne(file=NULL,lines=lines,type=tp,as.fun="data.table")[,par.type:=tp]
        
        lines[,NMrelateOne(lines=text,par.type=tp,as.fun="data.table"),by=col.model]
    })

    
    dt.relate <- rbindlist(list.relate)
    dt.relate[,par.type:=factor(par.type,levels=c("THETA","OMEGA","SIGMA"))]
    setorderv(dt.relate,c(col.model,"par.type"))
    dt.relate[,par.type:=as.character(par.type)]

    as.fun(dt.relate)  
}


