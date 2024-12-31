##' Calculate number of elements for matrix specification
##' 
##' calculate number of elements in the diagonal and lower triangle of
##' a matrix, based on the length of the diagonal
##'
##' @keywords internal

triagSize <- function(diagSize){
    ((diagSize^2)-diagSize)/2+diagSize
}
triagSize(1:5)

ifun <- function(blocksize,istart=1){
    rep(1:blocksize,times=1:blocksize)+istart-1
}
jfun <- function(blocksize,istart=1){
    unlist(lapply(1:blocksize,function(j) 1:j)) + istart-1
}
## ifun(3,istart=2)
## jfun(3,istart=2)



## Comprehensive regex pattern
dt.patterns <- data.table(
    name.pattern=c("block","ll.init.ul","ll.init","(init)","init","fix","same"),
    pattern=c("BLOCK\\s*(?:\\(\\d+\\))?",  # BLOCK(N)
              "\\(-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?,-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?,-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\)", # (ll,init,ul)
              "\\(-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?,-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\)", # (ll,init)
              "\\(-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\)",  # (init)
              "\\b-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\b",  # init (standalone number)
              "\\bFIX(ED)?\\b",  # FIX(ED)
              "SAME"
              )
)

patterns=c("block"="BLOCK\\s*(?:\\s*\\(\\d+\\s*\\))?",  # BLOCK(N)
           "ll.init.ul"="\\(\\s*-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*,\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*,\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*\\)", # (ll,init,ul)
           "ll.init"="\\(\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*,\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*\\)", # (ll,init)
           "(init)"="\\(\\s*-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*\\)",  # (init)
        ## "init"="\\b-?(?:(\\d+)?\\.?(\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\b",  # init (standalone number)
           ## "init"="\\b-?(\\d+\\.\\d+|\\d+\\.|\\.\\d+|\\d+|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\b",  # init (standalone number)
           ## "init"="\\b-?(\\d+\\.\\d+|\\d+\\.|\\.\\d+|\\d+)([eE][+-]?\\d+)?\\b",  # init (standalone number)
           ## "init"="\\b-?((\\d+\\.\\d+|\\d+\\.|\\.\\d+)([eE][+-]?\\d+)?|\\d+)(?!\\.)",
           ## "init"="\\b-?((\\d+\\.\\d+|\\d+\\.|\\.\\d+)([eE][+-]?\\d+)?|\\d+)(?!\\.)",
           "init" = "(?<!\\d)-?(\\d+\\.\\d+|\\d+\\.|\\.\\d+|\\d+)([eE][+-]?\\d+)?(?!\\.)",
           "fix"="\\bFIX(ED)?\\b",  # FIX(ED)
           "same"="SAME"
           )

use.pattern <- function(name){
    if(!name %in% names(patterns)){
        stop(paste("pattern called", name,"not found"))
    }
    paste0("^",patterns[[name]],"$")
}


classify_matches <- function(matches) {
    results <- list()
    
    ##pattern.singlenum <- "-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?"
    pattern.singlenum <- "-?(\\d+\\.\\d+|\\d+\\.|\\.\\d+|\\d)([eE][+-]?\\d+)?"
    for (match in matches) {
        ## if (grepl("^BLOCK\\(\\d+\\)$",match)) {
        if (grepl(use.pattern("block"),match)) {
            ## Extract the number from BLOCK(N)
            
            ## number <- as.numeric(str_match(match, "BLOCK\\((\\d+)\\)")[1, 2])
            if(grepl("\\( *(\\d+) *\\)",match)){
                number <- ## as.numeric(
                    stri_match_all_regex(match, "BLOCK\\( *(\\d+) *\\)")[[1]][1, 2]
                ##)
            } else {
                number <- "1"
            }
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "BLOCK",
                                           value.elem = number
                                       )))
        } else if (grepl(use.pattern("fix"),match)) {
            ## For FIX or FIXED, value is 1
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "FIX",
                                           value.elem = "1"
                                       )))
        } else if (grepl(use.pattern("ll.init.ul"),match)) {
            ## Split (ll,init,ul)
            ## nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
            nums <- ## as.numeric(
                stri_match_all_regex(match, pattern.singlenum)[[1]][, 1]
            ## )
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "ll",
                                           value.elem = nums[1]
                                       )))
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "init",
                                           value.elem = nums[2]
                                       )))
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "ul",
                                           value.elem = nums[3]
                                       )))
        } else if (grepl( use.pattern("ll.init"),match)) {
                                        # Split (ll,init)
            ## nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
            nums <- ## as.numeric(
                stri_match_all_regex(match, pattern.singlenum)[[1]][, 1]
            ## )
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "ll",
                                           value.elem = nums[1]
                                       )))
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "init",
                                           value.elem = nums[2]
                                       )))
        } else if (grepl( use.pattern("(init)"),match)) {
                                        # Extract init from (init)
            ## nums <- as.numeric(str_match(match, "-?\\d+(\\.\\d+)?")[1, 1])
            nums <- ## as.numeric(
                stri_match_all_regex(match, pattern.singlenum)[[1]][1, 1]
            ## )
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "init",
                                           value.elem = nums
                                       )))
        } else if (grepl( use.pattern("init"),match,perl=TRUE)) {
                                        # Standalone init
            ## nums <- as.numeric(match)
            nums <- match
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "init",
                                           value.elem = nums
                                       )))
        } else if (grepl( use.pattern("same"),match)) {
                                        # SAME
            nums <- match
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "init",
                                           value.elem = nums
                                       )))
        }
    }
    
    return(rbindlist(results))
}


##' @keywords internal

count_ij <- function(res){

    elem <- NULL
    inblock <- NULL
    elblock <- NULL
    does.count <- NULL
    blocksize <- NULL
    iblock <- NULL
    i <- NULL
    

    
    
    parcount <- 1
    icount <- 1
    dt.ij <- NULL
    while (parcount<=max(res[,parnum])){
        
        this.bsize <- 1

        if(res[parnum==parcount,unique(inblock)==TRUE]){
            
            
### assign i and j to all
            this.parblock <- res[parnum==parcount,unique(parblock)]
            this.res <- res[ parblock==this.parblock  & type.elem%in%c("init","ll","ul")]
            this.bsize <- this.res[,unique(blocksize)]

            ## res[parblock==this.parblock,i:=ifun(this.bsize,istart=icount)]
            
            this.dt.ij <- data.table(parnum=parcount+seq(0,triagSize(this.bsize)-1),
                                     i=ifun(this.bsize,istart=icount),
                                     j=jfun(this.bsize,istart=icount))
            
            ## res[parblock==this.parblock,j:=jfun(blocksize,istart=icount)]
            ## parcount <- parcount+triagSize(this.bsize)

        } else {
            ## res[parnum==parcount,i:=parcount]
            ## res[parnum==parcount,j:=parcount]
            this.dt.ij <- data.table(parnum=parcount,i=icount,j=icount)
            ## parcount <- parcount+1
        }

        dt.ij <- rbind(dt.ij,this.dt.ij)
        icount <- icount + this.bsize
        parcount <- parcount+triagSize(this.bsize)
    }

    
    res <- mergeCheck(res,dt.ij,by="parnum",all.x=TRUE,quiet=TRUE)

    ## browser()
    if(any(res[,inblock])){
        res[inblock==TRUE,iblock:=min(i,na.rm=TRUE),by=parblock]
    }
    if(any(!res[,inblock])){
        res[inblock==FALSE,iblock:=i]
    }
    
    res[,c("lastblockmax","inblock"):=NULL]
    res
}


##' Identify active elements in a parameter section
##' @import stringi
##' @keywords internal
processLines <- function(lines,section,debug=FALSE) {

    elemnum <- NULL
    string <- NULL
    . <- NULL
    linenum <- NULL
    does.count <- NULL
    type.elem <- NULL
    elem <- NULL
    elblock <- NULL
    blocksize <- NULL
    value.elem <- NULL
    lastblockmax <- NULL
    inblock <- NULL
    par.type <- NULL
    
    if(missing(section)) section <- NULL
    if(is.null(section)) {
        stop("section must be supplied") 
    }

    section <- sub("\\$","",section)
    section <- cleanSpaces(section)
    section <- toupper(section)

    if(length(section)>1) stop("Only one section can be handled at a time.")
    ## We want to keep everything, even empty lines so we can keep track of line numbers
    lines <- NMreadSection(lines=lines,section=section,keep.empty=TRUE,keep.comments=TRUE)
    
    if(length(lines)==0) return(NULL)

    
    pattern <- paste(patterns,collapse="|")
    
    ## Preprocess to remove comments (everything after ";")
    ## lines_cleaned <- str_replace(lines, ";.*", "")
    lines_cleaned <- gsub( ";.*", "",lines)
    
                                        # Extract matches
    ## matches <- str_extract_all(lines_cleaned, pattern)
    
    matches <- stri_extract_all_regex(lines_cleaned, pattern)
    
    ## Function to classify matches and insert NA where applicable



    matches.list <- lapply(seq_along(matches),function(I){
        match <- matches[[I]]
        if(length(match)==0) return(NULL)
        data.table(linenum=I,string=match)
    })
    dt.match <- rbindlist(matches.list)

    ## elemnum counts the fidings. It is an arbitrary counter because it groups (ll,init,ul) together but not FIX. It really can't be used for anything beyond this function so should not be exported.
    dt.match[,elemnum:=.I]

    
    res <- dt.match[,classify_matches(string),by=.(linenum,elemnum)]

    ## count parameter number - init,ll,ul,FIX will all be assigned to one parameter number
    res[,parnum:=NA_integer_]
    this.parnum <- 0
    prev.type <- "init"
    for(r in 1:nrow(res)){
        this.type <- res[r,type.elem]
                                        #  if(this.type!="BLOCK") {
                                        # }
        if( this.type == "BLOCK" ||
            (this.type == "ll" && prev.type!="BLOCK" ) ||
            (this.type == "init" && !prev.type %in% c("BLOCK","ll")) ){
            this.parnum <- this.parnum + 1
        }
        res[r,parnum:=this.parnum]
        prev.type <- this.type
    }
    
    ##res[,blocksize:=1]
    res[type.elem=="BLOCK",parblock:=as.integer(parnum)]
    res[type.elem=="BLOCK",blocksize:=as.integer(value.elem)]
    res[type.elem=="BLOCK",lastblockmax:=triagSize(blocksize)+parnum-1]
    res[,lastblockmax:=nafill(lastblockmax,type="locf")]

    if(F){
        res[,does.count:=TRUE]
        res[type.elem%in%c("BLOCK","FIX"),does.count:=FALSE]
        res[,elem:=elemnum]
        res[!does.count==TRUE,elem:=NA]
        res[,elem:=nafill(elem,type="nocb")]
        res[,elem:=nafill(elem,type="locf")]
        res[,elem:=.GRP,by=elem]
    }
    

    ## assign i,j

    res[,inblock:=FALSE]
    res[parnum<=lastblockmax,inblock:=TRUE]
    res[inblock==TRUE,blocksize:=nafill(blocksize,type="locf")]
    res[inblock==FALSE,blocksize:=1]
    res[inblock==TRUE,parblock:=nafill(parblock,type="locf")]

    res <- count_ij(res)
    res[,parblock:=NULL]
    res[,par.type:=section]
    
    


    res[]
}

