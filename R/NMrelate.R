##' Relate parameter names and variables based on control stream code
##' sections.
##' @param file Path to a control stream to process. See `lines` too.
##' @param lines If the control stream has been read already, the text
##'     can be provided here instead of using the `file`
##'     argument. Character vector of text lines.
##' @param par.type Parameter type(s) to include. Default is all three
##'     possible which is \code{c("THETA","OMEGA","SIGMA")}.
##' @param modelname Either a model name (like "Base") or a function
##'     that derives the model name from the control stream file
##'     path. The default is dropping the file name extension on the
##'     control stream file name.
##' @param col.model Name of the column containing the model name.
##' @param sections Sections of the control stream to
##'     consider. Default is all of \code{c("PRED","PK","ERROR")}.
##' @param as.fun The default is to return data as a data.frame. Pass
##'     a function (say tibble::as_tibble) in as.fun to convert to
##'     something else. If data.tables are wanted, use
##'     as.fun="data.table". The default can be configured using
##'     NMdataConf.
##' @details `NMrelate()` processes $PRED, $PK and $ERROR sections. It
##'     does not read ext files or $THETA, $OMEGA, $SIGMA sections to
##'     gain information but only extracts what it can from the model
##'     code. You can then merge with information from functions such
##'     as `NMreadExt()` and `NMreadParText()`.
##' @return data.frame relating parameters to variable names
##' @export

NMrelate <- function(file,lines,modelname,par.type,col.model,sections,as.fun){
    
    text <- NULL
    
    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL

    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)
    if(missing(par.type)) par.type <- NULL
    if(is.null(par.type)) par.type <- cl("THETA","OMEGA","SIGMA")    

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)


    lines <- getLines(file=file,lines=lines,col.model=col.model,modelname=modelname,as.one=TRUE)
    
    list.relate <- lapply(par.type,function(tp) {
        lines[,NMrelateOne(lines=text,par.type=tp,as.fun="data.table"),by=col.model]
    })

    
    dt.relate <- rbindlist(list.relate)
    dt.relate[,par.type:=factor(par.type,levels=c("THETA","OMEGA","SIGMA"))]
    setorderv(dt.relate,c(col.model,"par.type"))
    dt.relate[,par.type:=as.character(par.type)]

    as.fun(dt.relate)  
}


