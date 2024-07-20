### add par.type, i, j to a data.table that has parameter already
## parameter is style THETA1, OMEGA(1,1), SIGMA(1,1)

## should also add parname or par.name which is the concistent
## THETA(1), OMEGA(1,1), SIGMA(1,1) labeling.
addParType <- function(pars){
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
