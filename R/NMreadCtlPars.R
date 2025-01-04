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


itriag <- function(blocksize,istart=1,diag="lower"){
    rep(1:blocksize,times=1:blocksize)+istart-1
}
jtriag <- function(blocksize,istart=1,diag="lower"){
    unlist(lapply(1:blocksize,function(j) 1:j)) + istart-1
}
## itriag(3,istart=2)
## jtriag(3,istart=2)




patterns <- function(){
    c("block"="BLOCK\\s*(?:\\s*\\(\\d+\\s*\\))?",  # BLOCK(N)
           "ll.init.ul"="\\(\\s*-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*,\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*,\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*\\)", # (ll,init,ul)
           "ll.init"="\\(\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*,\\s*-?-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*\\)", # (ll,init)
           "(init)"="\\(\\s*-?(?:\\d+(\\.\\d+)?|(?:\\d+)?\\.\\d+)([eE][+-]?\\d+)?\\s*\\)",  # (init)
           "init" = "(?<!\\d)-?(\\d+\\.\\d+|\\d+\\.|\\.\\d+|\\d+)([eE][+-]?\\d+)?(?!\\.)",
           "fix"="\\bFIX(ED)?\\b",  # FIX(ED)
           "same"="SAME"
      )
}

use.pattern <- function(name){
    if(!name %in% names(patterns())){
        stop(paste("pattern called", name,"not found"))
    }
    paste0("^",patterns()[[name]],"$")
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
    blocksize <- NULL
    iblock <- NULL
    i <- NULL
    j <- NULL
    parnum <- NULL
    parblock <- NULL
    type.elem <- NULL
    
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

            this.dt.ij <- data.table(parnum=parcount+seq(0,triagSize(this.bsize)-1),
                                     i=itriag(this.bsize,istart=icount),
                                     j=jtriag(this.bsize,istart=icount))
            
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
##' @param lines A control stream as text lines
##' @param section The section to read. Typically, "theta", "omega", or "sigma".
##' @param as.fun See ?NMscanData
##' @import stringi
##' @keywords internal
##' @export 
NMreadCtlPars <- function(lines,section,as.fun) {

    . <- NULL
    blocksize <- NULL
    does.count <- NULL
    elblock <- NULL
    elem <- NULL
    elemnum <- NULL
    inblock <- NULL
    j <- NULL
    lastblockmax <- NULL
    linenum <- NULL
    parnum <- NULL
    parblock <- NULL
    par.type <- NULL
    string <- NULL
    type.elem <- NULL
    value.elem <- NULL
    
    if(missing(section)) section <- NULL
    if(is.null(section)) {
        stop("section must be supplied") 
    }

    if(missing(as.fun)) as.fun <- NULL
    as.fun <- NMdataDecideOption("as.fun",as.fun)
    
    section <- sub("\\$","",section)
    section <- cleanSpaces(section)
    section <- toupper(section)

    if(length(section)>1) stop("Only one section can be handled at a time.")
    ## We want to keep everything, even empty lines so we can keep track of line numbers
    lines <- NMreadSection(lines=lines,section=section,keep.empty=TRUE,keep.comments=TRUE)
    
    if(length(lines)==0) return(NULL)

    
    pattern <- paste(patterns(),collapse="|")
    
    ## Preprocess to remove comments (everything after ";")
    ## lines_cleaned <- str_replace(lines, ";.*", "")
    lines_cleaned <- gsub( ";.*", "",lines)

    ### rewrite (init FIX) as init FIX
    # Regular expression to match (init FIX) and (init FIXED)
    lines_cleaned <- gsub(
        pattern = "\\(\\s*(-?\\d+(\\.\\d+)?(?:[eE][+-]?\\d+)?)\\s+FIX(?:ED)?\\s*\\)",
        ## Replace with "init FIX"
        replacement = "\\1 FIX",
        x=lines_cleaned)
    
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
    res[par.type=="THETA",j:=NA]
    
    


    as.fun(res)
}

