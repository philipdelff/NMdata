## checks: duplicate column names

## has to be data.table

## col.flagn has to be taken from defaults

NMgenText <- function(data,nm.drop,
                      nmdir.data,col.flagn="FLAG", nm.rename,nm.copy,
                      file,
                      nm.capitalize=FALSE,allow.char.TIME=TRUE,
                      quiet=FALSE){

    

    if(missing(nmdir.data)) nmdir.data <- NULL
    if(missing(nm.drop)) nm.drop <- NULL
    if(missing(nm.rename)) nm.rename <- NULL
    if(missing(nm.copy)) nm.copy <- NULL
    if(missing(file)) file <- NULL

    
    ## only report numerics to user.
    ## Only report until first not interpretable as numeric. 
    as.num.ok <- data[,lapply(.SD,NMisNumeric)]

    
    ## Allow TIME even if non-numeric. 
    if(allow.char.TIME){
        if("TIME"%in%colnames(data) &&
           as.num.ok[,TIME==FALSE]) {
            as.num.ok[,TIME:=TRUE]
        }
    }


    dt.num.ok <- data.table(
        col=colnames(as.num.ok)
       ,numeric.ok=as.logical(as.num.ok[1])
    )



    ## if wanted, use only capital letters for column names in Nonmem
    dt.num.ok[,name.nm:=col]
    if(nm.capitalize){
        dt.num.ok[,name.nm:=toupper(name.nm)]
    }
    
    ## drop .'s from names since they are not allowed in nonmem
    ##    dt.num.ok[,name.nm:=gsub("\\.","",name.nm)]
    
    

    ## apply DROP
    dt.num.ok[,drop:=FALSE]
    if(!is.null(nm.drop)){
        dt.num.ok[col%in%nm.drop,name.nm:=paste0(name.nm,"=DROP")]
        dt.num.ok[col%in%nm.drop,drop:=TRUE]
        drops.not.used <- nm.drop[!nm.drop%in%dt.num.ok[,col]]
        if(length(drops.not.used)){
            warning("Elements in nm.drop not found as columns in data:",paste(drops.not.used,collapse=", "))
        }
    }
    
    ## apply nm.copy
    if(!is.null(nm.copy)){
        names.copy <- names(nm.copy)
        dt.num.ok[,name.copy:=names.copy[match(dt.num.ok[,col],nm.copy)]]
        dt.num.ok[!is.na(name.copy),name.nm:=paste0(name.copy,"=",col)]
    }

    

    ## apply nm.rename
    if(!is.null(nm.rename)){
        names.rename <- names(nm.rename)
        dt.num.ok[,name.rename:=names.rename[match(dt.num.ok[,col],nm.rename)]]
        dt.num.ok[!is.na(name.rename),name.nm:=name.rename]
    }
    
    
    
    dt.num.ok[,include:=cumsum(!numeric.ok&!drop)<1]

    dt.num.ok[include==TRUE,occ.cum:=1:.N,by=name.nm]
    if(dt.num.ok[occ.cum>1,.N]>0) {
        warning(paste("Duplicated column name(s) in data after transforming to upper case for Nonmem:\n",
                      paste0(dt.num.ok[occ.cum>1,unique(name.nm)],collapse=", "),"\n",
                      "Names have been numbered in $INPUT proposal."
                      ))
    }
    dt.num.ok[occ.cum>1,name.nm:=paste0(name.nm,occ.cum)]

    colnames.nm <- dt.num.ok[include==TRUE,name.nm]

    text.nm.input <- strwrap(
        paste0("$INPUT ",paste(colnames.nm,collapse=" "))
    )

    text.nm.data <- NULL
    if(!is.null(file)){
        
        text.nm.data <- c(paste0("$DATA ", file)
                         ,paste0("IGN=@")
                          )

        if(!is.null(col.flagn)&&col.flagn%in%colnames.nm){
            text.nm.data <- c(text.nm.data,
                              paste0("IGNORE=(",col.flagn,".NE.0)")
                              )
        }
    }
    
    text.nm <- c(
        text.nm.input
       ,text.nm.data
    )

    if(!quiet){
        message("For NonMem:\n",
                paste(text.nm,collapse="\n"))
    }

    list(INPUT=text.nm.input
        ,DATA=text.nm.data
         )


}
