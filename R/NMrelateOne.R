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

    ##if(type%in%cc(ETA,THETA)){
    lines <- c(NMreadSection(lines=lines,section="PRED",keep.comments=FALSE),
               NMreadSection(lines=lines,section="PK",keep.comments=FALSE),
               NMreadSection(lines=lines,section="ERROR",keep.comments=FALSE)
               )
    ## }
    ## if(type=="SIGMA"){
    ##    lines <- NMreadSection(lines=lines,section="ERROR",keep.comments=FALSE)
    ## }

    ## function(line,var.type){
    ##     ## eta, theta
    ##     if(var.type%in%cc(ETA,THETA,ERR,EPS)){
    ##         dt.code <- dt.code[,.(text.eta=regmatches(line2, gregexpr(paste0(str.regex,"\\(([0-9]+)\\)"),line2)) |> unlist()),by=.(line.eta,line2)]

    ##         dt.code[,i:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*)\\)"),"\\1",text.eta))]
    ##         if(type%in%c("ETA","ERR","EPS")){
    ##             dt.code[,j:=i]
    ##         } else {
    ##             dt.code[,j:=NA_integer_]
    ##         }
    ##     }
    ##     ## sigma
    ##     if(var.type=="SIGMA"){
    ##         dt.code <- dt.code[,.(text.eta=regmatches(line2, gregexpr(paste0(str.regex,"\\([0-9]+,[0-9]+\\)"),line2)) |> unlist()),by=.(line.eta,line2)]
    
    ##         dt.code[,i:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*),([1-9][0-9]*)\\)"),"\\1",text.eta))]
    ##         dt.code[,j:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*),([1-9][0-9]*)\\)"),"\\2",text.eta))]
    ##     }
    ## }
    
    
    
    dt.code <- data.table(line.eta = lines[grepl(str.regex.find,lines)])
    ## remove spaces
    dt.code[,line2:=gsub(" ","",line.eta)]

    ## determine found variable type?
    
    dt.code <- dt.code[,.(varname=regmatches(line2, gregexpr(paste0(str.regex,"\\(([0-9]+(,[0-9]+)*)\\)"),line2)) |> unlist()),by=.(line.eta,line2)]
    dt.code[,var.type:=sub(sprintf("(%s).*",str.regex),"\\1",varname)][,lineno:=.I]

###### instead, create a function that will be run on each line of dt.code
    ## eta, theta
    ##if(var.type%in%cc(ETA,THETA,ERR,EPS)){
                                        #dt.code <- dt.code[,.(parname=regmatches(line2, gregexpr(paste0(str.regex,"\\(([0-9]+)\\)"),line2)) |> unlist()),by=.(line.eta,line2)]
    
    ## dt.code[var.type%in%cc(ETA,THETA,ERR,EPS),i:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*)\\)"),"\\1",parname))]

    dt.code[var.type%in%cc(ETA,THETA,ERR,EPS),
            i:=as.numeric(sub(paste0(".*",var.type,"\\(([1-9][0-9]*)\\)"),"\\1",varname)),
            by=lineno]

    dt.code[,j:=NA_integer_]
    dt.code[var.type%in%c("ETA","ERR","EPS"),j:=i]

    ##}
    ##    }
    ## sigma
    
    dt.code[var.type=="SIGMA",
            ##dt.code <- dt.code[,.(parname=regmatches(line2, gregexpr(paste0(str.regex,"\\([0-9]+,[0-9]+\\)"),line2)) |> unlist()),by=.(line.eta,line2)]
            
            ##dt.code[,i:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*),([1-9][0-9]*)\\)"),"\\1",parname))]
            
            i:=as.integer(sub(".*\\(([1-9][0-9]*),([1-9][0-9]*)\\)","\\1",varname))]
    dt.code[var.type=="SIGMA",
            j:=as.integer(sub(".*\\(([1-9][0-9]*),([1-9][0-9]*)\\)","\\2",varname))]
    ##}


    dt.code[,parname:=varname]
    dt.code[var.type=="ETA",parname:=sprintf("%s(%s,%s)","OMEGA",i,j)]
    dt.code[var.type%in%cc(EPS,ERR),parname:=sprintf("%s(%s,%s)","SIGMA",i,j)]
    dt.code[,LHS:=sub("(.*)=.*","\\1",line2)]
    dt.code[,par.type:=..par.type]

    
    
    ## if a LHS is affected by multiple ETAs we number them
    dt.code[,nrep.LHS:=.N,by=.(LHS)]
    dt.code[,nrep.par:=.N,by=.(par.type,i,j)]
    dt.code[,label.LHS:=LHS]
    dt.code[nrep.LHS>1,label.LHS:=paste0(label.LHS," - ",par.type,"(",i,")")]
    
    if(by.par){
        dt.code.eta <- dt.code[,.(LHS=paste(unique(LHS),collapse=", "),
                                  label.LHS=paste(unique(LHS),collapse=", "),
                                  code=paste(line2,collapse=", ")
                                  ),by=.(parname,par.type,i,j,nrep.LHS)]

        dt.code.eta[nrep.LHS>1,
                    label.LHS:=paste(paste(unique(LHS),collapse=", "),parname,sep=" - ")]
        
    }


    
    ## dt.code.eta[,par.type:=switch(type,
    ##                               THETA="THETA",
    ##                               ETA="OMEGA",
    ##                               SIGMA="SIGMA",
    ##                               ERR="SIGMA")]

    
    
    setorder(dt.code.eta,i,j)
    ## dt.code.eta

    as.fun(dt.code.eta)

}
