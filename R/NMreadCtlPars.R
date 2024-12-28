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




classify_matches <- function(matches) {
    results <- list()
    
    for (match in matches) {
        if (grepl("^BLOCK\\(\\d+\\)$",match)) {
            ## Extract the number from BLOCK(N)
            
            ## number <- as.numeric(str_match(match, "BLOCK\\((\\d+)\\)")[1, 2])
            number <- as.numeric(stri_match_all_regex(match, "BLOCK\\((\\d+)\\)")[[1]][1, 2])
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "BLOCK",
                                           value.elem = number
                                       )))
        } else if (grepl("^FIX|FIXED$",match)) {
            ## For FIX or FIXED, value is 1
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "FIX",
                                           value.elem = 1
                                       )))
        } else if (grepl( "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$",match)) {
            ## Split (ll,init,ul)
            ## nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
            nums <- as.numeric(stri_match_all_regex(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
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
        } else if (grepl( "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$",match)) {
                                        # Split (ll,init)
            ## nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
            nums <- as.numeric(stri_match_all_regex(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
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
        } else if (grepl( "^\\(-?\\d+(\\.\\d+)?\\)$",match)) {
                                        # Extract init
            ## nums <- as.numeric(str_match(match, "-?\\d+(\\.\\d+)?")[1, 1])
            nums <- as.numeric(stri_match_all_regex(match, "-?\\d+(\\.\\d+)?")[[1]][1, 1])
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "init",
                                           value.elem = nums
                                       )))
        } else if (grepl( "^-?\\d+(\\.\\d+)?$",match)) {
                                        # Standalone init
            nums <- as.numeric(match)
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
    
    
    elcount <- 1
    icount <- 1
    dt.ij <- NULL
    while (elcount<=max(res[,elem])){
        
        this.bsize <- 1

        if(res[elem==elcount,unique(inblock)==TRUE]){
            
            
### assign i and j to all
            this.elblock <- res[elem==elcount,unique(elblock)]
            this.res <- res[ elblock==this.elblock  & does.count==TRUE]
            this.bsize <- this.res[,unique(blocksize)]

            ## res[elblock==this.elblock,i:=ifun(this.bsize,istart=icount)]
            
            this.dt.ij <- data.table(elem=this.res[,unique(elem)],
                                     i=ifun(this.bsize,istart=icount),
                                     j=jfun(this.bsize,istart=icount))
            
            ## res[elblock==this.elblock,j:=jfun(blocksize,istart=icount)]
            elcount <- elcount+this.dt.ij[,length(unique(elem))]

        } else {
            ## res[elem==elcount,i:=elcount]
            ## res[elem==elcount,j:=elcount]
            this.dt.ij <- data.table(elem=elcount,i=icount,j=icount)
            elcount <- elcount+1
        }

        dt.ij <- rbind(dt.ij,this.dt.ij)
        icount <- icount + this.bsize

    }

    
    res <- mergeCheck(res,dt.ij,by="elem",all.x=TRUE,quiet=TRUE)

    ## browser()
    if(any(res[,inblock])){
        res[inblock==TRUE,iblock:=min(i),by=elblock]
    }
    if(any(!res[,inblock])){
        res[inblock==FALSE,iblock:=i]
    }
    
    res[,c("lastblockmax","inblock","elblock"):=NULL]
    res
}


##' Identify active elements in a parameter section
##' @import stringi
##' @keywords internal
processLines <- function(lines,section,debug=FALSE) {

    row.elem <- NULL
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
    if(!is.null(section)) {
        section <- sub("\\$","",section)
        section <- cleanSpaces(section)
        section <- toupper(section)

        if(length(section)>1) stop("Only one section can be handled at a time.")
        ## We want to keep everything, even empty lines so we can keep track of line numbers
        lines <- NMreadSection(lines=lines,section=section,keep.empty=TRUE,keep.comments=TRUE)
    }
    if(length(lines)==0) return(NULL)
    
    ## Comprehensive regex pattern
    pattern <- paste(
        "BLOCK\\(\\d+\\)",                         # BLOCK(N)
        "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init,ul)
        "\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)", # (ll,init)
        "\\(-?\\d+(\\.\\d+)?\\)",                  # (init)
        "\\b-?\\d+(\\.\\d+)?\\b",                  # init (standalone number)
        "\\bFIX\\b",                               # FIX
        "\\bFIXED\\b",                             # FIXED
        sep = "|"
    )
    
    ## Preprocess to remove comments (everything after ";")
    ## lines_cleaned <- str_replace(lines, ";.*", "")
    lines_cleaned <- gsub( ";.*", "",lines)
    
                                        # Extract matches
    ## matches <- str_extract_all(lines_cleaned, pattern)
    matches <- stri_extract_all_regex(lines_cleaned, pattern)
    
    ## Function to classify matches and insert NA where applicable



    res.list <- lapply(seq_along(matches),function(I){
        match <- matches[[I]]
        if(length(match)==0) return(NULL)
        data.table(linenum=I,string=match)
    })
    res <- rbindlist(res.list)


    
    res[,row.elem:=.I]

    ## browser()
    
    res <- res[,classify_matches(string),by=.(linenum,row.elem)]

################ STOP somethings wrong. What is elem if element does not count? Instead of locf/nocb, I think this should be done by min(elem,na.rm=T),by=elblock.

    res[,does.count:=TRUE]
    res[type.elem%in%c("BLOCK","FIX"),does.count:=FALSE]
    res[,elem:=row.elem]
    res[!does.count==TRUE,elem:=NA]
    res[,elem:=nafill(elem,type="nocb")]
    res[,elem:=nafill(elem,type="locf")]
    
    res[,elem:=.GRP,by=elem]

    
    ##res[,blocksize:=1]
    res[type.elem=="BLOCK",elblock:=as.integer(elem)]
    res[type.elem=="BLOCK",blocksize:=as.integer(value.elem)]
    res[type.elem=="BLOCK",lastblockmax:=triagSize(blocksize)+elem-1]
    res[,lastblockmax:=nafill(lastblockmax,type="locf")]

    ## assign i,j

    res[,inblock:=FALSE]
    res[elem<=lastblockmax,inblock:=TRUE]
    res[inblock==TRUE,elblock:=nafill(elblock,type="locf")]
    res[inblock==TRUE,blocksize:=nafill(blocksize,type="locf")]
    res[inblock==FALSE,blocksize:=1]

    res <- count_ij(res)
    res[,par.type:=section]

    res[]
}

