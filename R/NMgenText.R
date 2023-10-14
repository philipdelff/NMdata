##' Generate text for INPUT and possibly DATA sections of NONMEM
##' control streams. 
##' 
##' The user is provided with text to use in Nonmem. NMwriteSection
##' can use the results to update the control streams. INPUT lists
##' names of the data columns while DATA provides a path to data and
##' ACCEPT/IGNORE statements. Once a column is reached that Nonmem
##' will not be able to read as a numeric and column is not in
##' nm.drop, the list is stopped. Only exception is TIME which is not
##' tested for whether character or not.
##'
##' @param data The data that NONMEM will read.
##' @param drop Only used for generation of proposed text for INPUT
##'     section. Columns to drop in Nonmem $INPUT. This has two
##'     implications. One is that the proposed $INPUT indicates =DROP
##'     after the given column names. The other that in case it is a
##'     non-numeric column, succeeding columns will still be included
##'     in $INPUT and can be read by NONMEM.
##' @param col.flagn Name of a numeric column with zero value for rows
##'     to include in Nonmem run, non-zero for rows to skip. The
##'     argument is only used for generating the proposed $DATA text
##'     to paste into the Nonmem control stream. To skip this feature,
##'     use col.flagn=NULL. Default is defined by NMdataConf.
##' @param rename For the $INPUT text proposal only. If you want to
##'     rename columns in NONMEM $DATA, NMwriteData can adjust the
##'     suggested $DATA text. If you plan to use BBW instead of BWBASE
##'     in Nonmem, consider rename=c(BBW="BWBASE"). The result will
##'     include BBW and not BWBASE.
##' @param copy For the $INPUT text proposal only. If you plan to use
##'     additional names for columns in Nonmem $INPUT, NMwriteData can
##'     adjust the suggested $INPUT text. Say you plan to use CONC as
##'     DV in Nonmem, use copy=c(DV="CONC"),
##'     i.e. copy=c(newname="existing"). INPUT suggestion will in this
##'     case contain DV=CONC.
##' @param file The file name NONMEM will read the data from (for the
##'     $DATA section). It can be a full path.
##' @param dir.data For the $DATA text proposal only. The path to the
##'     input datafile to be used in the Nonmem $DATA section. Often,
##'     a relative path to the actual Nonmem run is wanted here. If
##'     this is used, only the file name and not the path from the
##'     file argument is used.
##' @param capitalize For the $INPUT text proposal only. If TRUE, all
##'     column names in $INPUT text will be converted to capital
##'     letters.
##' @param until Use this to truncate the columns in $INPUT. until can
##'     either be a character (column name) or a numeric (column
##'     number). If a character is given, it is matched against the
##'     resulting column name representation in $INPUT, i.e. this
##'     could be "DV=CONC" if you are using in this case the copy
##'     argument. In case until is of length>1, the maximum will be
##'     used (probably only interesting if character values are
##'     supplied).
##' @param allow.char.TIME For the $INPUT text proposal only. Assume
##'     Nonmem can read TIME even if it can't be translated to
##'     numeric. This is necessary if using the 00:00 format. Default
##'     is TRUE.
##' @param width If positive, will be passed to strwrap for the $INPUT
##'     text. If missing or NULL, strwrap will be called with default
##'     value. If negative or zero, strwrap will not be called.
##' @param quiet Hold messages back? Default is defined by NMdataConf.
##' @return Text for inclusion in Nonmem control stream, invisibly. A
##'     list with elements `DATA` and `INPUT`.
##' @family Nonmem
##' @export

NMgenText <- function(data,
                      drop,
                      col.flagn="FLAG",
                      rename,
                      copy,
                      file,
                      dir.data,
                      capitalize=FALSE,
                      until,
                      allow.char.TIME=TRUE,
                      width,
                      quiet
                      ){
    
#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####    
    occ.cum <- NULL
    TIME <- NULL
    name.nm <- NULL
    drop.nm <- NULL
    name.pseudo <- NULL
    name.rename <- NULL
    include <- NULL
    numeric.ok <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

### we deal with copy first because it interferes with data.table::copy. 
    if(missing(copy)) copy <- NULL
    pseudo <- copy
    rm(copy)

    
    if(!is.data.frame(data)) messageWrap("data must be a data.frame",fun.msg=stop)    
    data <- copy(as.data.table(data))

    
    if(missing(col.flagn)) col.flagn <- NULL
    col.flagn <- NMdataDecideOption("col.flagn",col.flagn)
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(dir.data)) dir.data <- NULL
    if(missing(drop)) drop <- NULL
    if(missing(rename)) rename <- NULL
    
    if(missing(file)) file <- NULL

    if(!is.null(drop) && !is.character(drop) ) {
        stop("If supplied, drop must be of type character.")
    }
    if(any(is.na(drop)|drop=="")){
        stop("drop cannot contain empty strings and NA's.")
    }
    if(missing(width)) width <- NULL
    
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
    
    ## apply "until"
    if(!missing(until) && !is.null(until)){
        if(!is.numeric(until)&&!is.character(until)){
            messageWrap("until must be either numeric or character.")
        }
        if(is.character(until)){
            ## convert to numeric
            until <- match(until,dt.num.ok[,name.nm])
        }
        until <- max(until)
        dt.num.ok <- dt.num.ok[1:until]
    }

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
    if(dt.num.ok[,sum(include)]==0) {
        warning("No columns to use in Nonmem. Did you forget to order or drop columns")
        return(NULL)
    }
    
    dt.num.ok[include==TRUE,occ.cum:=1:.N,by=name.nm]
    if(dt.num.ok[occ.cum>1,.N]>0) {
        warning(paste("Duplicated column name(s) in data for Nonmem:\n",
                      paste0(dt.num.ok[occ.cum>1,unique(name.nm)],collapse=", "),"\n",
                      "Names have been numbered in $INPUT proposal."
                      ))
    }
    dt.num.ok[occ.cum>1,name.nm:=paste0(name.nm,occ.cum)]

    colnames.nm <- dt.num.ok[include==TRUE,name.nm]

    text.nm.input <- paste0("$INPUT ",paste(colnames.nm,collapse=" "))
    ## wrap if width is missing or positive
    if(is.null(width)){
        text.nm.input <- strwrap(
            text.nm.input
        )
        } else if(width>0){
        text.nm.input <- strwrap(
            text.nm.input
           ,width=width
        )
    }

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
        message("For NONMEM:\n",
                paste(text.nm,collapse="\n"))
    }

    list(INPUT=text.nm.input
        ,DATA=text.nm.data
         )


}
