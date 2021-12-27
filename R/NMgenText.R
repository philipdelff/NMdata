## checks: duplicate column names

## ok has to be data.table

## OK quiet has to be taken from defaults

## test if a pseudonym was used. Give warning or msg if not

## how do I control quiet from NMwriteData? Same as for NMwriteData I guess

NMgenText <- function(data,
                      drop,
                      dir.data,
                      col.flagn="FLAG",
                      rename
                     ,pseudo,
                      file,
                      capitalize=FALSE
                     ,allow.char.TIME=TRUE
                     ,quiet){

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####    
    occ.cum <- NULL

### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    if(!is.data.frame(data)) messageWrap("data must be a data.frame",fun.msg=stop)    
    data <- copy(as.data.table(data))

    
    if(missing(col.flagn)) col.flagn <- NULL
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(dir.data)) dir.data <- NULL
    if(missing(drop)) drop <- NULL
    if(missing(rename)) rename <- NULL
    if(missing(pseudo)) pseudo <- NULL
    if(missing(file)) file <- NULL

    if(!is.null(drop) && !is.character(drop) ) {
        stop("If supplied, drop must be of type character.")
    }
    if(any(is.na(drop)|drop=="")){
        stop("drop cannot contain empty strings and NA's.")
    }

    
### capitalize, allow.char.TIME have to be logical
    

    file.nm <- file

    if(!is.null(dir.data)&&!is.null(file.nm)){
        file.nm <- file.path(dir.data,basename(file.nm))
    } else if(is.null(file.nm)){
        file.nm <- "<data file>"
    }
    
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
    if(capitalize){
        dt.num.ok[,name.nm:=toupper(name.nm)]
    }
    
    ## drop .'s from names since they are not allowed in nonmem
    ##    dt.num.ok[,name.nm:=gsub("\\.","",name.nm)]
    
    

    ## apply DROP
    dt.num.ok[,drop.nm:=FALSE]
    if(!is.null(drop)){
        
        dt.num.ok[col%in%drop,name.nm:=paste0(name.nm,"=DROP")]
        dt.num.ok[col%in%drop,drop.nm:=TRUE]
        drops.not.used <- drop[!drop%in%dt.num.ok[,col]]
        if(length(drops.not.used)){
            warning("Elements in drop not found as columns in data:",paste(drops.not.used,collapse=", "))
        }
    }
    
    ## apply pseudo
    if(!is.null(pseudo)){
        names.pseudo <- names(pseudo)
        dt.num.ok[,name.pseudo:=names.pseudo[match(dt.num.ok[,col],pseudo)]]
        dt.num.ok[!is.na(name.pseudo),name.nm:=paste0(name.pseudo,"=",col)]
    }

    

    ## apply rename
    if(!is.null(rename)){
        names.rename <- names(rename)
        dt.num.ok[,name.rename:=names.rename[match(dt.num.ok[,col],rename)]]
        dt.num.ok[!is.na(name.rename),name.nm:=name.rename]
    }
    
    
    
    dt.num.ok[,include:=cumsum(!numeric.ok&!drop.nm)<1]

    dt.num.ok[include==TRUE,occ.cum:=1:.N,by=name.nm]
    if(dt.num.ok[occ.cum>1,.N]>0) {
        warning(paste("Duplicated column name(s) in data for Nonmem:\n",
                      paste0(dt.num.ok[occ.cum>1,unique(name.nm)],collapse=", "),"\n",
                      "Names have been numbered in $INPUT proposal."
                      ))
    }
    dt.num.ok[occ.cum>1,name.nm:=paste0(name.nm,occ.cum)]

    colnames.nm <- dt.num.ok[include==TRUE,name.nm]

    text.nm.input <- strwrap(
        paste0("$INPUT ",paste(colnames.nm,collapse=" "))
    )

    text.nm.data <- c(paste0("$DATA ", file.nm)
                     ,paste0("IGN=@")
                      )

    if(!is.null(col.flagn)&&col.flagn%in%colnames.nm){
        text.nm.data <- c(text.nm.data,
                          paste0("IGNORE=(",col.flagn,".NE.0)")
                          )
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
