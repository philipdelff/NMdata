### add par.type, i, j to a data.table that has parameter already
## parameter is style THETA1, OMEGA(1,1), SIGMA(1,1)

## should also add parname or par.name which is the concistent
## THETA(1), OMEGA(1,1), SIGMA(1,1) labeling.
addParType <- function(pars,suffix,add.idx){

    col.parameter <- "parameter"
    col.par.type <- "par.type"
    if(missing(suffix)) suffix <- NULL
    if(!is.null(suffix)){
        col.parameter <- paste(col.parameter,suffix,sep=".")
        col.par.type <- paste(col.par.type,suffix,sep=".")
    }

    if(missing(add.idx)) add.idx <- NULL
    if(is.null(add.idx)){
        add.idx <- is.null(suffix)
    }

    pars[,(col.par.type):=NA_character_]
    pars[grepl("^THETA",get(col.parameter)),(col.par.type):="THETA"]
    pars[grepl("^OMEGA",get(col.parameter)),(col.par.type):="OMEGA"]
    pars[grepl("^SIGMA",get(col.parameter)),(col.par.type):="SIGMA"]
    if(add.idx){
        pars[get(col.par.type)=="THETA",i:=sub("THETA([0-9]+)","\\1",get(col.parameter))]
        pars[get(col.par.type)=="OMEGA",i:=sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\1",get(col.parameter))]
        pars[get(col.par.type)=="OMEGA",j:=sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\2",get(col.parameter))]
        pars[get(col.par.type)=="SIGMA",i:=sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\1",get(col.parameter))]
        pars[get(col.par.type)=="SIGMA",j:=sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\2",get(col.parameter))]
        cols <- cc(i,j)
        pars[,(cols):=lapply(.SD,as.integer),.SDcols=cols]
    }
    pars[]
}
