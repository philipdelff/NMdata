##' Translate filters in Nonmem and apply to data
##' @param data An input data object. Could be read with NMreadCsv or
##'     NMtransInput.
##' @param file Path to mod/lst file. Only one of file, text, or lines to be
##'     given. See ?NMgetSection for understanding when to use, file, text, or
##'     lines.
##' @param text The mod/lst as characters.
##' @param lines The mod/lst as character, line by line.
##' @param debug start by running browser()?
##' @family Nonmem

## At least for now, don't export. This is still very experimental, and it is a
## function only being used by NMscanData at this point.


NMtransFilters <- function(data,file,text,lines,quiet=FALSE,debug=FALSE){
    if(debug) browser()

    ## stop("Translation of filters in nonmem control stream is not implemented. In order to make use of input data, you must include the same row counter in input and output data. At least for now.")
    
    ## get mod/lst text in lines format
    if(sum(c(!missing(file)&&!is.null(file),
             !missing(lines)&&!is.null(lines),
             !missing(text)&&!is.null(text)
             ))!=1) stop("Exactly one of file, lines, or text must be supplied")
    if(!missing(file)&&!is.null(file)) {
        if(!file.exists(file)) stop("When using the file argument, file has to point to an existing file.")
        lines <- readLines(file)
    }
    if(!missing(text)&&!is.null(text)) {
        lines <- strsplit(text,split="\n")[[1]]
    }

    ## If these are not NULL, it can make trouble in NMgetSection.
    file <- NULL
    text <- NULL
    
    ## this is not how to handle. Either file or remove comments and only look for IGN and ACCEPT
    text2 <- NMgetSection(lines=lines,section="DATA",keepComments=F)
    text3 <- sub(";.*$","",text2)

    ## check if IGNORE or ACCEPT are found. If both found, it is an error. 
    any.accepts <- any(grepl("ACCEPT",text3))
    any.ignores <- any(grepl("IGN",text3))
    ## if no filters found, just return data as is
    if(!any.accepts&&!any.ignores) return(data)
    if(any.accepts&&any.ignores) stop("IGNORE and ACCEPT are not allowed together according to Nonmem documentation.")

    ## If it's ignore, replace the allowed IGN with IGNORE
    if(any.ignores){
        text3 <- gsub("IGN([^O])","IGNORE\\1",text3)
        type.condition <- "IGNORE"
    } else {
        type.condition <- "ACCEPT"
    }
    

    ##### TODO: this is only for ignore. And is allowed to be combined with
    ##### accept lists.
    ## the single-chacter ones line @ or C. Here = is mandatory.
    conds.sc <- regmatches(text3, gregexpr(paste0(type.condition," *= *[^ (+]"),text3))
    conds.sc <- do.call(c,conds.sc)
    
### expression-style ones
    ## this is not correct.
### 1. A comma-separated list of expressions can be inside the ()s.
    ## 2. Expressions can be nested.
### 1. has to be handled, 2 can be detected and give an error - too complex to interpret.
    conds.expr <-
        regmatches(text3, gregexpr(paste0(type.condition," *=* *\\([^)]*\\)"),text3))
    conds.expr <- do.call(c,conds.expr)

    ## translating single-charaters
    name.c1 <- colnames(data)[1]
    scs <- sub(paste0(type.condition," *=* *(.+)"),"\\1",conds.sc)
    expressions <- c()
    if(length(scs)&&grepl("@",scs)) {
### NM manual: @ means first non-blank is a-z or A-Z.
        ## expressions <- c(expressions,paste0("!grepl(\"^[A-Z]|^[a-z]\",",name.c1,")"))
        expressions <- c(expressions,paste0("grepl(\"^ *[A-Za-z]\",",name.c1,")"))
        scs <- scs[!grepl("@",scs)]
    }
    
    if(length(scs)&&grepl("^a-zA-Z",scs)){
        scs2 <- regmatches(scs,regexpr("^[a-zA-Z]",scs))
        expressions <- c(expressions,paste0("!grepl('^",scs2,"\",",name.c1,")"))
        scs <- scs[!grepl("^[a-zA-Z]",scs)]
    }
    if(length(scs)) stop(paste0("Not all single-character IGNORE statements were translated. This is left: ",scs))

    ## translating expression-style ones
    conds.list <- strsplit(gsub(paste0(type.condition," *= *\\((.+)\\)"),"\\1",conds.expr),split=",")
    conds.char <- lapply(conds.list,c)
    expressions <- c(expressions,paste0("!",
                                        NMcode2R(
                                            conds.char
                                        )
                                        ))

## remember to negate everything if the type is ignore
    if(type.condition=="IGNORE") expressions <- paste0("!",expressions)
    
    expressions.all <- paste(expressions,collapse="&")
    if(!quiet) message(paste("Condition to apply:",expressions.all))
    data2 <- as.data.table(data)[eval(parse(text=expressions.all))]

    data2
    
    
}
