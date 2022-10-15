##' Translate filters in Nonmem and apply to data
##' @param data An input data object. Could be read with NMreadCsv or
##'     NMscanInput.
##' @param file Path to mod/lst file. Only one of file, text, or lines to be
##'     given. See ?NMreadSection for understanding when to use, file, text, or
##'     lines.
##' @param text The mod/lst as characters.
##' @param lines The mod/lst as character, line by line.
##' @param invert Invert the filters? This means read what Nonmem would
##'     disregard, and disregard what Nonmem would read.
##' @param as.fun The default is to return data as a data.frame. Pass a function
##'     (say tibble::as_tibble) in as.fun to convert to something else. If
##'     data.tables are wanted, use as.fun="data.table". The default can be
##'     configured using NMdataConf.
##' @param quiet Don't report information along the way if no warnings or
##'     errors. Default is FALSE.
##' @details This is not bulletproof. Nested conditions are not supported altogether.
##' @return data with filters applied
##' @keywords internal
##' @family Nonmem

## Don't export. This is only being used by NMscanInput at this point.


NMapplyFilters <- function(data,file,text,lines,invert=FALSE,as.fun,quiet) {

#### Section start: Dummy variables, only not to get NOTE's in pacakge checks ####

    . <- NULL
    variable <- NULL
    value <- NULL
    
### Section end: Dummy variables, only not to get NOTE's in pacakge checks

    
    if(missing(quiet)) quiet <- NULL
    quiet <- NMdataDecideOption("quiet",quiet)
    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    ## get mod/lst text in lines format
    if(sum(c(!missing(file)&&!is.null(file),
             !missing(lines)&&!is.null(lines),
             !missing(text)&&!is.null(text)
             ))!=1){
        messageWrap("Exactly one of file, lines, or text must be supplied",fun.msg=stop)
    }
    if(!missing(file)&&!is.null(file)) {
        if(!file.exists(file)) messageWrap("When using the file argument, file has to point to an existing file.",fun.msg=stop)
        lines <- readLines(file,warn=FALSE)
    }
    if(!missing(text)&&!is.null(text)) {
        lines <- strsplit(text,split="\n")[[1]]
    }

    ## If these are not NULL, it can make trouble in NMreadSection.
    file <- NULL
    text <- NULL

### We leave meta data untouched. This part is due to a previous design of NMscanInput. 
    
    data.meta <- NMinfoDT(data)

    text2 <- NMreadSection(lines=lines,section="DATA",keepComments=FALSE)
    text3 <- sub(";.*$","",text2)

    ## replace the allowed IGN with IGNORE
    ## the single-chacter ones line @ or C. Here = is mandatory.
    conds.sc <- regmatches(text3, gregexpr(paste0("IGN(?:ORE)"," *= *[^ (+]"),text3))
    conds.sc <- do.call(c,conds.sc)
### why is this 
    text3 <- gsub(paste0("IGNORE"," *= *[^ (+]"),"",text3)

    ## check if IGNORE or ACCEPT are found. If both found, it is an error. 
    any.accepts <- any(grepl("ACCEPT",text3))
    any.ignores <- any(grepl("IGN",text3))
    ## if no filters found, just return data as is
    if(!any.accepts&&!any.ignores&length(conds.sc)==0) return(data)
    if(any.accepts&&any.ignores) stop("IGNORE and ACCEPT are not allowed together according to Nonmem documentation.")
    
    if(any.ignores) {
        type.condition <- "IGNORE"
    } else {
        type.condition <- "ACCEPT"
    }
    

### expression-style ones
    ## this is not entirely correct.
### 1. A comma-separated list of expressions can be inside the ()s.
    ## 2. Expressions can be nested.
### 1. is handled below, 2 should be detected and give an error - interpretation not implemented.
    conds.expr <-
        regmatches(text3, gregexpr(paste0(type.condition," *=* *\\([^)]*\\)"),text3))
    conds.expr <- do.call(c,conds.expr)

    
    
    ## translating single-charaters
    name.c1 <- colnames(data)[1]
    scs <- sub(paste0("IGNORE"," *=* *(.+)"),"\\1",conds.sc)
    scs.all <- scs
    expressions.sc <- c()
    if(length(scs)&&grepl("@",scs)) {
### NM manual: @ means first non-blank is a-z or A-Z.
        expressions.sc <- c(expressions.sc,paste0("!grepl(\"^ *[A-Za-z]\",",name.c1,")"))
        scs <- scs[!grepl("@",scs)]
    }

    ##    regstring <- "^[a-zA-Z]"
### other single character ignores can be any character - except for space
    ##    regstring <- "[[:graph:]]"
    ##  regstring <- "([[:punct:]]|[[:alpha:]])"
    regstring <- "[[:punct:]]|[[:alpha:]]"

    
    if(length(scs)&&any(grepl(regstring,scs))) {
        
        scs2 <- regmatches(scs,regexpr(regstring,scs))
        ## expressions.sc <- c(expressions.sc,paste0("!grepl('^",scs2,"\",",name.c1,")"))
        ## expressions.sc <- c(expressions.sc,paste0("!grepl('^",scs2,"','",name.c1,"')"))
        expressions.sc <- c(expressions.sc,paste0("!grepl('^[",scs2,"]',`",name.c1,"`)"))
        scs <- scs[!grepl(regstring,scs)]
    }

    if(length(scs)) stop(paste0("Not all single-character IGNORE statements were translated. This is left: ",scs))
    
    ## translating expression-style ones
    conds.list <- strsplit(
        gsub(paste0(type.condition," *=* *\\((.+)\\)"),"\\1",conds.expr)
       ,split=",")
    conds.char <- do.call(c,conds.list)

    
    expressions.list <- c(paste0(
        NMcode2R(
            conds.char
        )
    ))

    ## replace single = with ==
    expressions.list <- sub("^([a-zA-Z]* *)=( *[0-9]+)$","\\1==\\2",expressions.list)
    ## (DOSE 10) means (DOSE==10) in NMTRAN. 
    expressions.list <- sub("([[:alpha:]]+) +([[:alnum:]]+)","\\1==\\2",expressions.list)

    vars.cond <- sub("([[:alnum:]])[^[:alnum:]]+.*","\\1",expressions.list)
    
    if(length(vars.cond)){
        missings <- listMissings(data,cols=unique(vars.cond),quiet=TRUE)
        if(!is.null(missings)&&nrow(missings)>0){
            warning(paste("Missing values found in columns used for ACCEPT/IGNORE statements. This is not supported. If at all possible, please use a unique row identifier to merge by and/or make sure values are not missing in these colums.\n",paste(capture.output(print(missings[,.N,by=.(variable,value)])),collapse="\n")))
            
        }
    }
    
    cond.combine <- "|"
    ## remember to negate everything if the type is ignore
    if(type.condition=="IGNORE") {
        expressions.list <- paste0("!",expressions.list)
        cond.combine <- "&"
    }
    
    if(length(expressions.sc)) {
        conditions.all.sc <- paste0(expressions.sc,collapse="&")
    } else {
        conditions.all.sc <- "TRUE"
    }
    
    expressions.all <- NULL
    if(length(expressions.list)) {
        expressions.all <- paste0("(",paste(expressions.list,collapse=cond.combine),")")
    }
    
    if(invert) {
        
        conditions.all.sc <- paste("!(",conditions.all.sc,")")
        if(!is.null(expressions.all)){
            expressions.all <- paste("!",expressions.all)
            data <- as.data.table(data)[eval(parse(text=paste(conditions.all.sc,"|",expressions.all)))]
        } else {
            data <- as.data.table(data)[eval(parse(text=conditions.all.sc))]
        }
        
    } else {
        ## apply sc first
        data <- as.data.table(data)[eval(parse(text=conditions.all.sc))]
        
        ## then lists
        if(!is.null(expressions.all)){
            data <- as.data.table(data)[eval(parse(text=expressions.all))]
        }
    }
    
    conds.text <- paste0(type.condition,": ",paste(conds.char,collapse=", "))
    data.meta.filters <- data.table(
        nonmem=c(conds.sc,
                 conds.text),
        R=c(conditions.all.sc,paste(expressions.all,collapse=", "))
    )
    if(nrow(data.meta.filters)){
        data.meta$input.filters <- data.meta.filters
    }
    
    data <- as.fun(data)
    
    writeNMinfo(data,meta=data.meta,append=TRUE)
    data
    
}
