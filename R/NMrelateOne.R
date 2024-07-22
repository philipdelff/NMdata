### the structure is a little off. argument should be par.type and take one of the values THETA, OMEGA, SIGMA. Then for par.type=SIGMA, look for SIGMA, EPS, ERR (var.type) and process depending on var.type is found

## if var.type is SIGMA look for i, j, if not look for i only. var.type OMEGA not supported.

## if par.type is SIGMA, var.type is EPS, ERR, set j:=i

###### Take all the naming based on varname and put in new function, some of it is covered by addParType already.
## that new function should also be called by NMreadShk


NMrelateOne <- function(file,lines,par.type="OMEGA",by.par=TRUE,as.fun){

    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)

    if(!by.par){stop("")}
    
    lines <- getLines(file=file,lines=lines)
    
    par.type <- toupper(cleanSpaces(par.type))
    if(par.type=="ETA") par.type <- "OMEGA"
    if(par.type%in%c("ERR","EPS")) par.type <- "SIGMA"

    str.regex <- switch(par.type,
                        OMEGA="ETA"
                       ,THETA="THETA"
                       ,SIGMA="(SIGMA|ERR|EPS)"
                        ##,ERR="(ERR|EPS)"
                        )
    str.regex.find <- paste0("[^[:alnum:]]",str.regex)

    lines <- c(NMreadSection(lines=lines,section="PRED",keep.comments=FALSE),
               NMreadSection(lines=lines,section="PK",keep.comments=FALSE),
               NMreadSection(lines=lines,section="ERROR",keep.comments=FALSE)
               )
    
    
    dt.code <- data.table(line.var = lines[grepl(str.regex.find,lines)])
    ## remove spaces
    dt.code[,line2:=gsub(" ","",line.var)]

    ## determine found variable type?
    
    dt.code <- dt.code[,.(varname=regmatches(line2, gregexpr(paste0(str.regex.find,"\\(([0-9]+(,[0-9]+)*)\\)"),line2)) |> unlist()),by=.(line.var,line2)]
    dt.code[,varname:=sub("^[^[:alnum:]]","",varname)]
    dt.code[,var.type:=sub(sprintf("(%s).*",str.regex),"\\1",varname)][,lineno:=.I]


    dt.code[var.type%in%cc(ETA,THETA,ERR,EPS),
            i:=as.numeric(sub(paste0(".*",var.type,"\\(([1-9][0-9]*)\\)"),"\\1",varname)),
            by=lineno]

    dt.code[,j:=NA_integer_]
    dt.code[var.type%in%c("ETA","ERR","EPS"),j:=i]

    dt.code[var.type=="SIGMA",            
            i:=as.integer(sub(".*\\(([1-9][0-9]*),([1-9][0-9]*)\\)","\\1",varname))]
    dt.code[var.type=="SIGMA",
            j:=as.integer(sub(".*\\(([1-9][0-9]*),([1-9][0-9]*)\\)","\\2",varname))]



    dt.code[,par.name:=varname]
    dt.code[var.type=="ETA",par.name:=sprintf("%s(%s,%s)","OMEGA",i,j)]
    dt.code[var.type%in%cc(EPS,ERR),par.name:=sprintf("%s(%s,%s)","SIGMA",i,j)]
    dt.code[,LHS:=sub("(.*)=.*","\\1",line2)]
    dt.code[,par.type:=..par.type]

    
    
    ## if a LHS is affected by multiple ETAs we number them
    dt.code[,nrep.LHS:=.N,by=.(LHS)]
    dt.code[,nrep.par:=.N,by=.(par.type,i,j)]
    dt.code[,label:=LHS]
    dt.code[nrep.LHS>1,label:=paste0(label," - ",par.type,"(",i,")")]
    
    if(by.par){
        dt.code <- dt.code[,.(LHS=paste(unique(LHS),collapse=", "),
                                  label=paste(unique(LHS),collapse=", "),
                                  code=paste(line2,collapse=", ")
                                  ),by=.(par.name,par.type,i,j,nrep.LHS,nrep.par)]

        dt.code[nrep.LHS>1,
                    label:=paste(paste(unique(LHS),collapse=", "),par.name,sep=" - ")]
        
    }

    
    
    setorder(dt.code,i,j)

    as.fun(dt.code)

}
