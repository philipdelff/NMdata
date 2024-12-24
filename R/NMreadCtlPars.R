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
        if (str_detect(match, "^BLOCK\\(\\d+\\)$")) {
                                        # Extract the number from BLOCK(N)
            number <- as.numeric(str_match(match, "BLOCK\\((\\d+)\\)")[1, 2])
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "BLOCK",
                                           value.elem = number
                                       )))
        } else if (str_detect(match, "^FIX|FIXED$")) {
                                        # For FIX or FIXED, value is 1
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "FIX",
                                           value.elem = 1
                                       )))
        } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$")) {
                                        # Split (ll,init,ul)
            nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
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
        } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?,-?\\d+(\\.\\d+)?\\)$")) {
                                        # Split (ll,init)
            nums <- as.numeric(str_match_all(match, "-?\\d+(\\.\\d+)?")[[1]][, 1])
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
        } else if (str_detect(match, "^\\(-?\\d+(\\.\\d+)?\\)$")) {
                                        # Extract init
            nums <- as.numeric(str_match(match, "-?\\d+(\\.\\d+)?")[1, 1])
            results <- append(results, list(list(
                                           string.elem = match,
                                           type.elem = "init",
                                           value.elem = nums
                                       )))
        } else if (str_detect(match, "^-?\\d+(\\.\\d+)?$")) {
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



count_ij <- function(res){
    elcount <- 1
    icount <- 1
    dt.ij <- NULL
    while (elcount<=max(res[,elem])){
        ## browser()        
        this.bsize <- 1
        if(res[elem==elcount,unique(inblock==TRUE)]){
            ## browser()
### assign i and j to all
            this.elblock <- res[elem==elcount,unique(elblock)]
            this.bsize <- res[elem==elcount,unique(blocksize)]

            ## res[elblock==this.elblock,i:=ifun(this.bsize,istart=icount)]
            
            this.dt.ij <- data.table(elem=res[elblock==this.elblock,unique(elem)],
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

    res <- mergeCheck(res,dt.ij,by="elem",all.x=TRUE)

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


processLines <- function(lines,debug=FALSE) {
    
                                        # Comprehensive regex pattern
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
    
                                        # Preprocess to remove comments (everything after ";")
    lines_cleaned <- str_replace(lines, ";.*", "")
    
                                        # Extract matches
    matches <- str_extract_all(lines_cleaned, pattern)
    
                                        # Function to classify matches and insert NA where applicable



    res.list <- lapply(seq_along(matches),function(I){
        match <- matches[[I]]
        if(length(match)==0) return(NULL)
        data.table(line=I,string=match)
    })
    res <- rbindlist(res.list)


    
    res[,row.elem:=.I]

    ## browser()
    
    res <- res[,classify_matches(string),by=.(line,row.elem)]



    res[,does.count:=TRUE]
    res[type.elem%in%c("BLOCK","FIX"),does.count:=FALSE]
    res[,elem:=row.elem]
    res[!does.count==TRUE,elem:=NA]
    res[,elem:=nafill(elem,type="locf")]
    res[,elem:=nafill(elem,type="nocb")]
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
    res[]
}

