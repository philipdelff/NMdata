##' translate the column names according to the $INPUT section of a control stream
##'
##' @param data.input the data to translate
##' @param file

## don't export. An internal function used by NMscanInput. 

NMtransInp <- function(data,file){

## data is assumed to be a data.table, and a data.table is returned.
    
    ## According to NM manual IV-1, $INPUT and $INFILE are the same thing.    
    lines <- NMreadSection(file,section="INPUT",keepName=FALSE,keepComments=FALSE,cleanSpaces=TRUE)
    if(is.null(lines)) {
        lines <- NMreadSection(file,section="INPT",keepName=FALSE,keepComments=FALSE,cleanSpaces=TRUE)
    }
    if(is.null(lines)) {stop("Could not find $INPUT or $INPT section in control stream. Cannot interpret data. Is file really the path to a valid nonmem control stream?")}
    
    ## names can be separated by , or " " or both. So one , between alphanumerics is replaced by a single space
    lines <- gsub("([[:alnum:]]) *, *([[:alnum:]])","\\1 \\2",lines)
    ## get rid of redundant spaces
    line <- gsub(" +"," ",paste(lines,collapse=" "))
    line <- sub("^ ","",line)
    line <- sub(" $","",line)

### nms is the names of columns as in nonmem control stream
    nms <- strsplit(line," ")[[1]]
    nms0 <- nms
    
    
### this is to keep even dropped columns
    nms <- sub("(.*) *= *(DROP|SKIP)","\\1",nms)
    ## For now, we just take the first name used in A=B labeling. 
    renamed.from <- NULL
    renamed.to <- NULL
    renamed.from <- sub("(.*)=(.*)","\\1",nms[grepl(".*=.*",nms)])
    renamed.to <- sub("(.*)=(.*)","\\2",nms[grepl(".*=.*",nms)])
    nms <- sub(".*=(.*)","\\1",nms)
    ## this is the names as read. Keep them for reporting.
    nms1 <- nms
    
    ## More column names can be specified in the nonmem control stream
    ## than actually found in the input data. We will simply disregard
    ## them.
    cnames.input.0 <- copy(colnames(data))
    cnames.input <- copy(cnames.input.0)
    if(length(nms)>length(cnames.input)){
        nms <- nms[1:length(cnames.input)]
        messageWrap("More column names specified in Nonmem $INPUT than found in data file. The additional names have been disregarded.",fun.msg=warning)
    }
    
    cnames.input[1:length(nms)] <- nms
    colnames(data) <- cnames.input

    ## add the pseudonyms
    if(!is.null(renamed.from)){
        data <- cbind(data,
                            setnames(data[,c(renamed.to),with=FALSE],old=renamed.to,new=renamed.from)
                            )
    }
    
    ## check for unique column names
    if(any(duplicated(cnames.input))) {
        nms2 <- cnames.input[-(1:length(nms))]
        if(any(duplicated(nms))){
            messageWrap(paste("Duplicated variable names declared in nonmem $INPUT section. Only first of the columns will be used:",paste(nms[duplicated(nms)],collapse=", ")),fun.msg=warning)
            ## nms.u <- unique(nms)
        } 
        if(length(nms2)&&any(duplicated(nms2))){
            messageWrap(paste("Duplicated variable names detected in input data not processed by Nonmem. Only first of the columns will be used:",paste(nms2[duplicated(nms2)],collapse=", ")),fun.msg=warning)
        }
        nms.cross <- c(unique(nms),unique(nms2))
        if(any(duplicated(nms.cross))){
            
            messageWrap(paste("The same variable names are found in input variables as read by nonmem and the rest of input data file. Please look at column names in input data and the $INPUT section in nonmem control stream. Only the first occurrence of the columns will be used:",paste(unique(nms.cross[duplicated(nms.cross)]),collapse=", ")),fun.msg=warning)
        }
#### Reduce to unique column names
        
        data <- data[,unique(cnames.input),with=F]
    }

        length.max <- max(length(cnames.input.0),
                              length(nms0),
                              length(nms1),
                              length(colnames(data))
                              )
    dt.colnames <- data.table(input=c(cnames.input.0,rep(NA_character_,length.max-length(cnames.input.0))),
                              nonmem=c(nms0,rep(NA_character_,length.max-length(nms0))),
                              result=c(nms1,rep(NA_character_,length.max-length(nms1))),
                              result.all=c(colnames(data),rep(NA_character_,length.max-length(colnames(data))))
                              )

    list(data=data,dt.colnames=dt.colnames)
}
