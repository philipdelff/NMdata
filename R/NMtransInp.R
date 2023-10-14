##' translate the column names according to the $INPUT section of a control stream
##'
##' @param data the data to translate
##' @param file the list file or control stream
##' @param translate logical. Do translation according to Nonmem code
##'     or not? If not, an overview of column names in data and in
##'     Nonmem code is still returned with the data.
##' @param recover.cols recover columns that were not used in the
##'     NONMEM control stream? Default is TRUE. Can only be negative
##'     when translate=FALSE.
##' @return data with column names translated as specified by nonmem
##'     control stream. Class same as for 'data' argument. Class
##'     data.table.
##' @import data.table
##' @keywords internal

## don't export. An internal function used by NMscanInput. 

NMtransInp <- function(data,file,translate=TRUE,recover.cols=TRUE){

#### Section start: Dummy variables, only not to get NOTE's in package checks ####
    datafile <- NULL
    DATA <- NULL
    compare <- NULL    
    INPUT <- NULL

### Section end: Dummy variables, only not to get NOTE's in package checks ####

    stopifnot(is.data.table(data))

#### this should be supported now
    ##    if( !translate && !recover.cols ) {messageWrap("recover.rows=FALSE is only allowed when translate=TRUE.",fun.msg=stop)}
    
    ## According to NM manual IV-1, $INPUT and $INFILE are the same thing.    
    lines <- NMreadSection(file,section="INPUT",keep.name=FALSE,keep.comments=FALSE,clean.spaces=TRUE)
    if(is.null(lines)) {
        lines <- NMreadSection(file,section="INPT",keep.name=FALSE,keep.comments=FALSE,clean.spaces=TRUE)
    }
    if(is.null(lines)) {stop("Could not find $INPUT or $INPT section in control stream. Cannot interpret data. Is file really the path to a valid nonmem control stream?")}
    
    ## names can be separated by , or " " or both. So one , between alphanumerics is replaced by a single space
    lines <- gsub("([[:alnum:]]) *, *([[:alnum:]])","\\1 \\2",lines)
    ## editors may include \t for a tidy view. Replace by space.
    lines <- gsub("\t"," ",lines)
    ## get rid of redundant spaces
    line <- gsub(" +"," ",paste(lines,collapse=" "))
    line <- sub("^ ","",line)
    line <- sub(" $","",line)
    ## not sure what DV =A would do but it may be valid, so reduce to DV=A
    line <- sub(" *= *","=",line)

    
### nms is the names of columns as in nonmem control stream
    nms <- strsplit(line," ")[[1]]
    nms0 <- nms
    
    
### this is to keep even dropped columns
    idx.drop <- grep("=(DROP|SKIP)",nms0)
    nms <- sub("(.*)=(DROP|SKIP)","\\1",nms)
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
    nminfo.data.0 <- NMinfoDT(data)
    cnames.input.0 <- copy(colnames(data))
    cnames.input <- copy(cnames.input.0)

    if(translate){
        if(length(nms)>length(cnames.input)){
            nms <- nms[1:length(cnames.input)]
            messageWrap("More column names specified in Nonmem $INPUT than found in data file. The additional names have been disregarded.",fun.msg=warning)
        }
        
        cnames.input[1:length(nms)] <- nms
        if(!recover.cols){
            data <- data[,1:length(nms)]
            cnames.input <- cnames.input[1:length(nms)]
        }
        colnames(data) <- cnames.input
        
        ## add the synononyms
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
            } 
            if(length(nms2)&&any(duplicated(nms2))){
                messageWrap(paste("Duplicated variable names detected in input data not processed by Nonmem. Only first of the columns will be used:",paste(nms2[duplicated(nms2)],collapse=", ")),fun.msg=warning)
            }
            nms.cross <- c(unique(nms),unique(nms2))
            if(any(duplicated(nms.cross))){
                
                messageWrap(paste("The same variable names are found in input variables as read by nonmem and the rest of input data file. Please look at column names in input data and the $INPUT section in nonmem control stream. Only the first occurrence of the columns will be used:",paste(unique(nms.cross[duplicated(nms.cross)]),collapse=", ")),fun.msg=warning)
            }
#### Reduce to unique column names
            
            data <- data[,unique(cnames.input),with=FALSE]
        }
    }

    ## if(!translate && !recover.cols){
    ##     data <- data[,1:length(nms)]
    ## }
    if(!recover.cols){
        data <- data[,1:length(nms)]
    }


    length.max <- max(length(cnames.input.0), ## datafile
                      length(nms0),       ## DATA
                      length(nms1),       ## nonmem
                      length(colnames(data)) ## result
                      )
    dt.colnames <- data.table(datafile=c(cnames.input.0,rep(NA_character_,length.max-length(cnames.input.0))),
                              ## DATA=c(nms0,rep(NA_character_,length.max-length(nms0))),
                              "INPUT"=c(nms0,rep(NA_character_,length.max-length(nms0))),
                              nonmem=c(nms1,rep(NA_character_,length.max-length(nms1))),
                              ## result.all=c(colnames(data),rep(NA_character_,length.max-length(colnames(data))))
                              result=c(colnames(data),rep(NA_character_,length.max-length(colnames(data))))
                              )

    
    ## compare: OK, diff, off
    dt.colnames[tolower(datafile)==tolower(INPUT),compare:="OK"]
    dt.colnames[tolower(datafile)!=tolower(INPUT),compare:="diff"]
    dt.colnames[compare=="diff"&tolower(INPUT)%in%tolower(datafile),compare:="off"]
    dt.colnames[,compare:=factor(compare,levels=c("OK","diff","off"))]
    
    
    writeNMinfo(data,nminfo.data.0,byRef=TRUE)
    writeNMinfo(data,list(input.colnames=dt.colnames),append=TRUE,byRef=TRUE)
    data
}
