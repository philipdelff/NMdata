##' Read information from Nonmem phi files
##'
##' @param file.phi Path to the phi file
##'
##' @return A list with a final parameter table and a table of the iterations
##' @export

NMreadPhi <- function(file.phi){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    par.type <- NULL
    parameter <- NULL
    i <- NULL
    j <- NULL
    
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)

    
    res.NMdat <- NMreadTab(file.phi,as.fun="data.table",quiet=TRUE)
    pars <- melt(res.NMdat,id.vars=c("SUBJECT_NO","ID","NMREP"),variable.name="parameter")

    pars[,par.type:=NA_character_]
    pars[grepl("^ETA",parameter),par.type:="ETA"]
    pars[grepl("^ETC",parameter),par.type:="ETC"]
    pars[parameter=="OBJ",par.type:="OBJ"]
    pars[par.type=="ETA",i:=sub("ETA\\(([0-9]+)\\)","\\1",parameter)]
    pars[par.type=="ETC",i:=sub("ETC\\(([0-9]+)\\,([0-9]+)\\)","\\1",parameter)]
    pars[par.type=="ETC",j:=sub("ETC\\(([0-9]+)\\,([0-9]+)\\)","\\2",parameter)]
    cols <- cc(i,j)
    pars[,(cols):=lapply(.SD,as.integer),.SDcols=cols]


    as.fun(pars)

}
