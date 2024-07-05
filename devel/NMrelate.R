NMrelate <- function(file.mod,type="eta"){

    type <- toupper(type)
    str.regex <- switch(type,
                        ETA="[^H]ETA"
                       ,THETA="THETA"
                       ,SIGMA="SIGMA")

    if(type%in%cc(ETA,THETA)){
        lines <- c(NMreadSection(file.mod,section="PRED",keep.comments=FALSE),
                   NMreadSection(file.mod,section="PK",keep.comments=FALSE)
                   )
    }
    if(type=="SIGMA"){
        lines <- NMreadSection(file.mod,section="ERROR",keep.comments=FALSE)
    }

    

    
    dt.code <- data.table(line.eta = lines[grepl(str.regex,lines)])
    ## remove spaces
    dt.code[,line2:=gsub(" ","",line.eta)]

    ## eta, theta
        if(type%in%cc(ETA,THETA)){
    dt.code <- dt.code[,.(text.eta=regmatches(line2, gregexpr(paste0(str.regex,"\\(([0-9]+)\\)"),line2)) |> unlist()),by=.(line.eta,line2)]

    dt.code[,i:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*)\\)"),"\\1",text.eta))]
        }
    ## sigma
    if(type=="SIGMA"){
        dt.code <- dt.code[,.(text.eta=regmatches(line2, gregexpr(paste0(str.regex,"\\([0-9]+,[0-9]+\\)"),line2)) |> unlist()),by=.(line.eta,line2)]
        
        dt.code[,i:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*),([1-9][0-9]*)\\)"),"\\1",text.eta))]
        dt.code[,j:=as.numeric(sub(paste0(".*",str.regex,"\\(([1-9][0-9]*),([1-9][0-9]*)\\)"),"\\2",text.eta))]
    }

    
    dt.code[,LHS:=sub("(.*)=.*","\\1",line2)]

    dt.code.eta <- dt.code[,.(label=paste(unique(LHS),collapse=", "),
                              code=paste(line2,collapse=", ")
                              ),by=.(i,LHS)]

    ## if a LHS is affected by multiple ETAs we number them
    dt.code.eta[,nrep.LHS:=.N,by=.(LHS)]
    dt.code.eta[nrep.LHS>1,label:=paste0(label," - ",type,"(",i,")")]
    
    dt.code.eta[order(i)]

}
