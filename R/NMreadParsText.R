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
##' @param unique.matches If TRUE, each line in the control stream is
##'     assigned to one parameter, at most. This means, if two
##'     parameters are listed in one line, the comments will only be
##'     used for one of the parameters, and only that parameter will
##'     be kept in output. Where this will typically happen is in
##'     `$OMEGA` and `$SIGMA` sections where off-diagonal may be put
##'     on the same line as diagonal elements. Since the off-diagonal
##'     elements are covariances of variables that have already been
##'     identified by the diagonals, the off-diagonal elements can be
##'     automatically described. For example, if `OMEGA(1,1)` is
##'     between-subject variability (BSV) on CL and `OMEGA(2,2) is BSV
##'     on V, then we know that `OMEGA(2,1)` is covariance of (BSV on)
##'     CL and V.
##' @param spaces.split Is a blank in `fields` to be treated as a
##'     field seperator? Default is not to (i.e. neglect spaces in
##'     `fields`).
##' @param use.idx The default method is to automatically identify
##'     element numbering (`i` for THETAs, `i` and `j` for OMEGAs and
##'     SIGMAs). The automated method is based on identification of
##'     `BLOCK()` structures and numbers of initial values. Should
##'     this fail, or should you want to control this manually, you
##'     can include a parameter counter in the comments and have
##'     `NMreadParsText()` use that to assign the
##'     numbering. `use.idx=FALSE` is default and means all blocks are
##'     handled automatically, `use.idx=TRUE` assumes you have a
##'     counter in all sections, and a character vector like
##'     `use.idx="omega"` can be used to denote which sections use
##'     such a counter from the control stream. When using a counter
##'     on OMEGA and SIGMA, off-diagonal elements MUST be denoted by
##'     `i-j`, like `2-1` for OMEGA(2,1). See `field.idx` too.
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
##' @param use.theta.idx If an index field in comments should be used
##'     to number thetas. The index field is used to organize
##'     `$OMEGA`s and `$SIGMA`s because they are matrices but I do not
##'     see where this is advantageous to do for `$THETA`s. Default
##'     `use.theta.idx=FALSE` which means `$THETA`s are simply
##'     counted.
##' @param fields Deprecated. Use `format`.
##' @param fields.omega Deprecated. Use `format.omega`.
##' @param fields.sigma Deprecated. Use `format.sigma`.
##' @return data.frame with parameter names and fields read from
##'     comments
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
                           unique.matches=TRUE,
                           field.idx="idx",
                           use.idx=FALSE,
                           modelname,
                           col.model,
                           as.fun,
                           use.theta.idx,
                           fields,
                           fields.omega=fields,
                           fields.sigma=fields.omega
                           ){

    . <- NULL
    idx <- NULL
    par.type <- NULL
    i <- NULL
    j <- NULL
    linenum <- NULL
    parameter <- NULL
    text <- NULL
    text.clean <- NULL
    type.elem <- NULL
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
            
            res <- lapply(file,function(f){
                args2 <- args
                args2$file <- f
                do.call(NMreadParsText,args2)
            })
            return(res)
        } else {
            lines <- readLines(file,warn=FALSE)
        }
        
    }

    if(! ( is.logical(use.idx) || is.character(use.idx)) ) stop("use.idx must be logical or a character vector containing a subset of c(\"theta\",\"omega\",\"sigma\").")
    if(is.logical(use.idx)){
        if(use.idx) {
            use.idx <- c("theta","omega","sigma")
        } else {
            use.idx <- c()
        }
    }

    if(missing(use.theta.idx)) use.theta.idx <- NULL
    if(!is.null(use.theta.idx)) {
        message("use.theta.idx is deprecated. Use `use.idx` argument instead.")
        if(!is.logical(use.theta.idx)) stop("use.theta.idx is deprecated. Use `use.idx` argument instead.")
        if(use.theta.idx) use.idx <- c("theta",use.theta.idx)
    }
    use.idx <- unique(toupper(use.idx))

    
    if(any(!use.idx %in% c("THETA","OMEGA","SIGMA"))){
        stop("use.idx must be logical or a character vector containing a subset of c(\"theta\",\"omega\",\"sigma\").")
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
        lines.thetas <- NMreadSection(lines=lines,section=section,keep.name=TRUE,keep.empty=TRUE,keep.comments=TRUE)
        if(length(lines.thetas)==0) return(NULL)
        
        dt.lines <- data.table(text=lines.thetas)
        dt.lines[,linenum:=.I]
        dt.lines[,text.clean:=text]
### We want to keep comments - that's what we want to process
        dt.lines[,text.clean:=sub("^ *;.*","",text.clean)]
        dt.lines[,text.clean:=sub(paste0("\\$",section),"",text.clean,ignore.case=TRUE)]
        dt.lines[,text.clean:=gsub("BLOCK(.+)","",text.clean)]
        dt.lines[,text.clean:=sub("^ *$","",text.clean)]

        

        ### lapply is needed because the data table way fails when results have different numbers of columns
        ## dt.pars <- dt.lines[text.clean!="",fun.get.fields(text.clean,res.fields),by=linenum]
        dt.lines.reduced <- dt.lines[text.clean!=""]

        
        
        dt.pars <- rbindlist(lapply(1:nrow(dt.lines.reduced),function(Nrow){
            fun.get.fields(dt.lines.reduced[Nrow,text.clean],res.fields)[,linenum:=dt.lines.reduced[Nrow,linenum]]
        }),
            fill=TRUE)
        colnames(dt.pars) <- c(res.fields$fields[1:(ncol(dt.pars)-1)],"linenum")
        
        if(F){
            ## Remove empty lines and lines that are comments only. NMreadSection() does not have a way to do this.
            lines.thetas <- sub(pattern="^ *;.*$",replacement="",x=lines.thetas)
            ## these will confuse in omega/sigma sections with the current method. For those, numbering has to be done if off-diag elements are defined.
            lines.thetas <- gsub("BLOCK(.+)","",lines.thetas)
            lines.thetas <- lines.thetas[!grepl("^ *$",lines.thetas)]

            thetas.list <- lapply(lines.thetas,fun.get.fields,res.fields)
            thetas <- rbindlist(thetas.list,fill=TRUE)
            colnames(thetas) <- res.fields$fields[1:ncol(thetas)]

        }
        


        dt.pars[,par.type:=toupper(section)]

        if(use.theta.idx && field.idx%in%colnames(theta)){
            dt.pars[,i:=get(field.idx)]
            rm.idx <<- FALSE
        } else {
            dt.pars[,i:=.I]
        }
        dt.pars
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
    
    
#### get.omega.comments must return line numbers
    ## why is idx there? 
    omegas <- get.omega.comments(lines=lines,section="omega",format=format.omega)
    sigmas <- get.omega.comments(lines=lines,section="sigma",format=format.sigma)

    auto.idx <- setdiff(c("THETA","OMEGA","SIGMA"),use.idx)
    
    
    if(length(sigmas)==0) auto.idx <- setdiff(auto.idx,"SIGMA")
    
    if("THETA"%in%auto.idx){
        
        elems.theta <- NMreadCtlPars(lines,section="THETA",as.fun="data.table")
        thetas <- merge(thetas[,setdiff(colnames(thetas),c("i","j")),with=FALSE],
                        elems.theta[type.elem=="init",.(par.type,linenum,i)],
                        by=c("par.type","linenum"),all.x=TRUE)
    }
    if("OMEGA"%in%auto.idx){
        
        elems.omega <- NMreadCtlPars(lines,section="OMEGA",as.fun="data.table")
        omegas <- merge(omegas[,setdiff(colnames(omegas),c("i","j")),with=FALSE],
                        elems.omega[type.elem=="init",.(par.type,linenum,i,j)],
                        by=c("par.type","linenum"),all.x=TRUE)
    }
    if("SIGMA"%in%auto.idx){
        
        elems.sigma <- NMreadCtlPars(lines,section="SIGMA",as.fun="data.table")
        sigmas <- merge(sigmas[,setdiff(colnames(sigmas),c("i","j")),with=FALSE],
                        elems.sigma[type.elem=="init",.(par.type,linenum,i,j)],
by=c("par.type","linenum"),all.x=TRUE)
    }


    
    
    ## collect thetas and omegas
    pt1 <- rbind(thetas,omegas,sigmas,fill=T)

######## Isn't this because the user is doing it wrongly?
    ## we are aligning with Nonmem's behavior in ext lines. The
    ## off-diags are lower triangle. In other words, OMEGA and SIGMA
    ## are specified by column, not by row.
    ## pt1[par.type%in%cc(OMEGA,SIGMA),`:=`(i=j,j=i)]
    
    
####### TODO: what if multiple elements are on one line? Assign comment to all or to the diagonals only? Maybe an argument to control.
#### merges ij,j on each line. Risk is that single lines contain multiple elements, and hence are matched by multiple combinations of i,j.
###        pt1 <- merge(pt1[,setdiff(colnames(pt1),c("i","j")),with=FALSE],elems.all[type.elem=="init"],by=c("par.type","linenum"),all.x=TRUE)
#### Optional: Prioritizes lines: one i,j per line. order by i=j, then i, then j. Take unique (first occurrance).
    ## if(length(auto.idx)){
    if(unique.matches){
        ## pt1[,rorig:=.I]
        ## pt1 <- pt1[frank(match(pt1$par.type,c("THETA","OMEGA","SIGMA")),i==j,i,j))
        ## pt.idx <- pt1[!par.type%in%auto.idx]
        ## pt.auto
        
        pt1 <- pt1[order(match(par.type,c("THETA","OMEGA","SIGMA")),i!=j,i,j)]
        pt2 <- unique(pt1,by=c("par.type","linenum"))
        pt2 <- pt2[order(match(par.type,c("THETA","OMEGA","SIGMA")),i,j)]
        ## setkey(pt1,par.type,i)
        ## unique(pt1)
        pt1 <- pt2
    }
    ## }


    

### it is inconsistent, but this is how it is reported in NMreadExt
    pt1[par.type%in%cc(THETA),parameter:=sprintf("THETA%d",i)]
    pt1[par.type%in%cc(OMEGA,SIGMA),parameter:=sprintf("%s(%d,%d)",par.type,i,j)]
    
    if(rm.idx) pt1[,(field.idx):=NULL]

    if(!is.null(file)){
        this.model <- modelname(file)
        pt1[,(col.model):=this.model]
    }

    if("linenum" %in% colnames(pt1)) pt1[,linenum:=NULL]
    
    cols.last <- intersect(c("par.type","i","j","col.idx","parameter",col.model),colnames(pt1))
    setcolorder(pt1,c(setdiff(colnames(pt1),cols.last),cols.last))
    
    
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
