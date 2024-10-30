##' Read comments to parameter definitions in Nonmem control streams
##'
##' When interpreting parameter estimates, it is often needed to
##' recover information about the meaning of the different parameters
##' from control stream. `NMreadParsText` provides a flexible way to
##' organize the comments in the parameter sections into a
##' `data.frame`. This can subsequently easily be merged with parameter
##' values as obtained with `NMreadExt`.
##' 
##' @param file Path to the control stream to read.
##' @param lines As an alternative to `file`, the control stream or
##'     selected lines of the control stream can be provided as a
##'     vector of lines.
##' @param format Defines naming and splitting of contents of lines in
##'     parameter sections. Default is
##'     \code{"\%init;\%idx;\%symbol;\%label;\%unit"}. Be careful to
##'     remember percentage symbols in front of any variable names.
##' @param format.omega Like `fields`, applied to `$OMEGA`
##'     section. Default is to reuse `fields`.
##' @param format.sigma Like `fields`, applied to `$SIGMA`
##'     section. Default is to reuse `fields.omega`.
##' @param use.theta.idx If an index field in comments should be used
##'     to number thetas. The index field is used to organize
##'     `$OMEGA`s and `$SIGMA`s because they are matrices but I do not
##'     see where this is advantageous to do for `$THETA`s. Default
##'     `use.theta.idx=FALSE` which means `$THETA`s are simply
##'     counted.
##' @param spaces.split Is a blank in `fields` to be treated as a
##'     field seperator? Default is not to (i.e. neglect spaces in
##'     `fields`).
##' @param field.idx If an index field is manually provided in the
##'     control stream comments, define the name of that field in
##'     `format` and tell `NMreadParsTab()` to use this idx to
##'     organize especially OMEGA and SIGMA elements by pointing to it
##'     with `field.idx`. The default is to look for a varible called
##'     `idx`. If the index has values like 1-2 on an OMEGA or SIGMA
##'     row, the row is interpreted as the covariance between
##'     OMEGA/SIGMA 1 and 2.
##' @param modelname See ?NMscanData
##' @param col.model See ?NMscanData
##' @param as.fun See ?NMscanData
##' @param fields Deprecated. Use `format`.
##' @param fields.omega Deprecated. Use `format.omega`.
##' @param fields.sigma Deprecated. Use `format.sigma`.
##' @return data.frame with parameter names and fields read from comments
##' @details Off-diagonal omega and sigma elements will only be
##'     correctly treated if their num field specifies say 1-2 to
##'     specify it is covariance between 1 and 2.
##'
##' \code{SAME} elements in \code{$OMEGA} will be skipped altogether.
##' @examples
##'
##' 
##' ## notice, examples on explicitly stated lines. Most often in
##' ## practice, one would use the file argument to automatically
##' ## extract the $THETA, $OMEGA and $SIGMA sections from a control
##' ## stream.
##'
##' text <- c("
##'
##' $THETA  (.1)             ;[1]; LTVKA (mL/h)
##' $OMEGA  BLOCK(3)
##' 0.126303  ;    IIV.CL  ; 1   ;IIV     ;Between-subject variability on CL;-
##' 0.024  ; IIV.CL.V2.cov  ; 1-2 ;IIV     ;Covariance of BSV on CL and V2;-
##' 0.127  ;    IIV.V2  ; 2   ;IIV     ;Between-subject variability on V2;-
##' 0.2  ; IIV.CL.V3.cov  ; 1-3 ;IIV     ;Covariance of BSV on CL and V3;-
##' 0.2  ; IIV.V2.V3.cov  ; 2-3 ;IIV     ;Covariance of BSV on V2 and V3;-
##' 0.38  ;    IIV.V3  ; 3   ;IIV     ;Between-subject variability on V3;-
##' $OMEGA 0 FIX ; IIV.KA ; 4  ;IIV     ;Between-subject variability on KA;-
##' $SIGMA 1
##' ")
##'    lines <- strsplit(text,split="\n")[[1]]
##'
##' res <- NMreadParsText(lines=lines,
##' format="%init;[%num];%symbol",
##' format.omega="%init; %symbol ; %num ; %type   ; %label ; %unit",
##' field.idx="num")
##'
##' ## BLOCK() SAME are skipped
##' text <- c("
##' $THETA
##' (0,0.1) ; THE1      - 1) 1st theta
##' (0,4.2) ; THE2        - 2) 2nd theta
##' $OMEGA  0.08   ;    IIV.TH1  ; 1  ;IIV
##' $OMEGA  BLOCK(1)
##' 0.547465  ; IOV.TH1  ; 2 ;IOV
##' $OMEGA  BLOCK(1) SAME
##' $OMEGA  BLOCK(1) SAME")
##' lines <- strsplit(text,split="\n")[[1]]
##' res <- NMreadParsText(lines=lines,
##'                          format="%init;%symbol - %idx) %label",
##'                          format.omega="%init; %symbol  ; %idx  ; %label "
##'                          )
##' @export


NMreadParsText <- function(file,lines,format,
                           format.omega=format,format.sigma=format.omega,
                           spaces.split=FALSE,
                           field.idx="idx",
                           use.theta.idx=FALSE,
                           modelname,col.model,as.fun,
                           fields,fields.omega=fields,
                           fields.sigma=fields.omega
                           ){

    idx <- NULL
    par.type <- NULL
    i <- NULL
    j <- NULL
    parameter <- NULL
    THETA <- NULL
    theta <- NULL
    OMEGA <- NULL
    SIGMA <- NULL

    if(missing(file)) file <- NULL
    if(missing(lines)) lines <- NULL
    
    if(!xor(is.null(file),is.null(lines))){
        stop("Exactly one of file and lines must be provided.")
    }

    ## args <- getArgs()
    args <- getArgs(sys.call(),parent.frame())

    if(!is.null(file)){
        if(length(file)>1) {
            
            ## return(rbindlist(lapply(file,NMreadParsText,
            ##                         fields=fields,
            ##                         fields.omega=fields.omega,
            ##                         fields.sigma=fields.sigma,
            ##                         use.theta.idx=use.theta.idx,
            ##                         spaces.split=spaces.split)))
            res <- lapply(file,function(f){
                args2 <- args
                args2$file <- f
                do.call(NMreadParsText,args2)
            })
            return(res)
        } else {
            lines <- readLines(file)
        }
        
    }
    
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    if(missing(col.model)) col.model <- NULL 
    col.model <- NMdataDecideOption("col.model",col.model)
    if(missing(modelname)) modelname <- NULL
    modelname <- NMdataDecideOption("modelname",modelname)


    if(missing(format)){
        format <- NULL
    }
    
    ## deprecated since 2024-07-09 - v0.1.7
    if(!missing(fields)) {
        .Deprecated(new="format",old="fields")
        format <- fields
    }
    if(!missing(fields.omega)) {
        .Deprecated(new="format.omega",old="fields.omega")
        format.omega <- fields.omega
    }
    if(!missing(fields.sigma)) {
        .Deprecated(new="format.sigma",old="fields.sigma")
        format.sigma <- fields.sigma
    }

    if(is.null(format)){
        format <- "%init;%idx;%symbol;%label;%unit"
    }
    if(is.function(format)) format <- format(lines)
    if(is.function(format.omega)) format.omega <- format.omega(lines)
    if(is.function(format.sigma)) format.sigma <- format.sigma(lines)

    cleanSpaces <- function(x,double=TRUE,lead=TRUE,trail=TRUE){
        if(double) x <- gsub(paste0(" +")," ",x)
        if(lead) x <- sub(paste0("^ +"),"",x)
        if(trail) x <- sub(paste0(" +$"),"",x)
        x
    }

    
    ## function to extact however many format are available in a commented
    ## parameter row
    fun.get.fields <- function(x,format){
        
        ## pat <- gsub("%[a-zA-Z0-9]*","(.*)",format)

        splitters <- format$splitters
        ## splitters.un <- unique(splitters)
        
        nfields.x <- 0
        xleft <- x

        ## nsplitters.x <- length(fields$splitters)
        nsplitters.x <- length(format$splitters)
        
        ## need a regex that matches the number of items found
        xleft <- x
        items <- c()
        
        for(I in 1:nsplitters.x){
            spl <- format$splitters[I]
            spl.raw <- format$splitters.raw[I]
            ## this.item.all <- sub(sprintf("([^%s]*)%s.*",
            ##                              splitters[I],
            ##                              splitters[I]),
            ##                      "\\1",xleft)

            this.item.all <- sub(sprintf("%s.*",splitters[I]), "", xleft)
            
            ## this.item <- sub("([^ ]*) $","\\1",this.item.all)
            this.item <- cleanSpaces(this.item.all)
            
            
            if(I<nsplitters.x){
                
                spl.raw.next <- format$splitters.raw[I]
                delim.partial <- paste(paste(strsplit(gsub("\\*","",spl.raw.next),"")[[1]],"*",sep=""),collapse="")
                delim.partial <- escape.charclass(delim.partial)
                this.item <- sub(sprintf("%s$",delim.partial),"",this.item)
            }
            items <- c(items,this.item)

            if(nchar(this.item.all)>0){
                xleft <- sub(paste0(this.item.all),"",xleft,fixed=TRUE)
            }
            xleft <- sub(paste0("^",splitters[I]),"",xleft)
            ##if(!grepl("[[:alnum:]]",xleft)) xleft <- ""
            if(xleft=="")  break
        }
        do.call(data.table,as.list(items))
    }
    
    



    get.theta.comments <- function(lines,section,format,use.theta.idx=FALSE){

        res.fields.theta <- splitFields(format,spaces.split=spaces.split)
        get.comments(lines,section,res.fields=res.fields.theta,use.theta.idx=FALSE)
    }

### applies processed fields on control stream sections. It does so by calling fun.get.fields
    get.comments <- function(lines,section,res.fields,use.theta.idx=FALSE){
        
        ## get theta comments
        lines.thetas <- NMreadSection(lines=lines,section=section,keep.name=FALSE,keep.empty=FALSE,keep.comments=TRUE)
        if(length(lines.thetas)==0) return(NULL)
        ## Remove empty lines and lines that are comments only. NMreadSection() does not have a way to do this.
        lines.thetas <- sub(pattern="^ *;.*$",replacement="",x=lines.thetas)
        ## these will confuse in omega/sigma sections with the current method. For those, numbering has to be done if off-diag elements are defined.
        lines.thetas <- gsub("BLOCK(.+)","",lines.thetas)
        lines.thetas <- lines.thetas[!grepl("^ *$",lines.thetas)]

        thetas.list <- lapply(lines.thetas,fun.get.fields,res.fields)
        thetas <- rbindlist(thetas.list,fill=TRUE)
        colnames(thetas) <- res.fields$fields[1:ncol(thetas)]

        thetas[,par.type:=toupper(section)]

        if(use.theta.idx && field.idx%in%colnames(theta)){
            thetas[,i:=get(field.idx)]
            rm.idx <<- FALSE
        } else {
            thetas[,i:=.I]
        }
    }

### notice, get.omega.comments uses omega as a placeholder for OMEGA or SIGMA
    get.omega.comments <- function(lines,section,format){
        
        res.fields <- splitFields(format,spaces.split=spaces.split)
        omegas <- get.comments(lines=lines,section=section,res.fields=res.fields)
        if(is.null(omegas)) return(NULL)
        if(field.idx%in%colnames(omegas)) {
            rm.idx <<- FALSE
        } else {
            omegas[,(field.idx):=.I]
        }
        omegas[,i:=NA_integer_]
        omegas[,j:=NA_integer_]
        
        omegas[grepl("^ *[0-9]+ *$",get(field.idx)),i:=as.integer(get(field.idx))]
        omegas[grepl("^ *[0-9]+ *$",get(field.idx)),j:=as.integer(get(field.idx))]
        omegas[grepl("-",get(field.idx)),i:=as.integer(sub(" *([0-9])+ *-.*","\\1",get(field.idx)))]
        omegas[grepl("-",get(field.idx)),j:=as.integer(sub(".*- *([0-9])","\\1",get(field.idx)))]
        omegas[,par.type:=toupper(section)]
        omegas
    }

    
    
    rm.idx <- TRUE    
    thetas <- get.theta.comments(lines=lines,section="THETA",format=format,
                                 use.theta.idx=use.theta.idx)
    omegas <- get.omega.comments(lines=lines,section="omega",format=format.omega)
    sigmas <- get.omega.comments(lines=lines,section="sigma",format=format.sigma)

    ## collect thetas and omegas
    pt1 <- rbind(thetas,omegas,sigmas,fill=T)
    ## we are aligning with Nonmem's behavior in ext liness. The
    ## off-diags are lower triangle. In other words, OMEGA and SIGMA
    ## are specified by column, not by row.
    pt1[par.type%in%cc(OMEGA,SIGMA),`:=`(i=j,j=i)]

### it is inconsistent, but this is how it is reported in NMreadExt
    pt1[par.type%in%cc(THETA),parameter:=sprintf("THETA%d",i)]
    pt1[par.type%in%cc(OMEGA,SIGMA),parameter:=sprintf("%s(%d,%d)",par.type,i,j)]
    
    if(rm.idx) pt1[,(field.idx):=NULL]

    cols.last <- intersect(c("par.type","i","j","col.idx","parameter","model"),colnames(pt1))
    setcolorder(pt1,c(setdiff(colnames(pt1),cols.last),cols.last))
    
    if(!is.null(file)){
        this.model <- modelname(file)
        pt1[,(col.model):=this.model]
    }
    
    as.fun(pt1)
}


##' @keywords internal
escape.charclass <- function(x) {
    gsub("([][\\\\^(\\\\^-])", "\\\\\\1", x)
}


##' splitFields splits the fields format string into the splitters
##' and the variable names. NB, this is interpreting the user-provided
##' fields, it is not looking at control stream text,
##' @keywords internal
splitFields <- function(format,spaces.split=FALSE){


    
    ## number of fields defined in format
    nfields.string <- nchar(gsub("[^%]","",format))
    ## number of fields provided in string
    if(!spaces.split){
        format <- gsub(" +","",format)
    }
    
    ## a string where variables are not %var but just % so we can find splitters.
    ## splitters <- gsub("%[[:alnum:]]+","",fields)
    string.splitters <- gsub("%[[:alnum:]]+","%",format)
    splitters <- strsplit(string.splitters,"%")[[1]][-1]
    
    if(!spaces.split) splitters <- unlist(lapply(strsplit(splitters, ""),function(x)paste(paste0(x," *"),collapse="")))
    ## adding an extra splitter for the last regex to find match
    splitters <- c(splitters,";")
    splitters.raw <- splitters
    splitters <- escape.charclass(splitters)

    
    string.vars <- gsub("[^[:alnum:]]+",";",format)
    vars <- strsplit(string.vars,";")[[1]][-1]

    list(fields=vars,splitters=splitters,splitters.raw=splitters.raw)
}
