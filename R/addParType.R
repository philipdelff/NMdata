### add par.type, i, j to a data.table that has parameter already
## parameter is style THETA1, OMEGA(1,1), SIGMA(1,1)

## should also add parname or par.name which is the concistent
## THETA(1), OMEGA(1,1), SIGMA(1,1) labeling.

##' @keywords internal
addParType <- function(pars,suffix,add.idx){

    i <- NULL
    j <- NULL
    par.name <- NULL
    parameter <- NULL
    
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
    pars[get(col.parameter)%in%cc("OBJ","SAEMOBJ"),(col.par.type):="OBJ"]
    if(add.idx){
        pars[get(col.par.type)=="THETA",i:=as.integer(sub("THETA([0-9]+)","\\1",get(col.parameter)))]
        pars[get(col.par.type)=="OMEGA",i:=as.integer(sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\1",get(col.parameter)))]
        pars[get(col.par.type)=="OMEGA",j:=as.integer(sub("OMEGA\\(([0-9]+)\\,([0-9]+)\\)","\\2",get(col.parameter)))]
        pars[get(col.par.type)=="SIGMA",i:=as.integer(sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\1",get(col.parameter)))]
        pars[get(col.par.type)=="SIGMA",j:=as.integer(sub("SIGMA\\(([0-9]+)\\,([0-9]+)\\)","\\2",get(col.parameter)))]
        ## cols <- cc(i,j)
        ## pars[,(cols):=lapply(.SD,as.integer),.SDcols=cols]
    }
## par.name
    pars[,par.name:=get(col.parameter)]
    pars[get(col.par.type)=="THETA",par.name:=sprintf("THETA(%s)",i)]
    
    
    pars[]
}
